{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Custom class to encapsulate the general purpose
queries that we need for building transactions
-}
module Q2io.Query (
  MonadUtxoQuery (..),
  utxosByPayment,
  balanceTx,

  -- * Tx balancing for operator
  balanceAndSubmitOperator,
  balancePaymentCredential,
  balancePaymentCredentials,
  signTxOperator,
  signAndSubmitOperator,
  operatorUtxos,
  selectOperatorUTxO,
  BalanceAndSubmitError (..),

  -- * Wallet API queries
  WalletAPIQueryT (..),
  runWalletAPIQueryT,
) where

import Cardano.Api (
  BabbageEra,
  BalancedTxBody,
  BuildTx,
  CtxTx,
  PaymentCredential (..),
  TxBodyContent,
  TxOut,
  UTxO (..),
 )
import Cardano.Api qualified as C
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Except.Result (ResultT)
import Control.Monad.Trans.State qualified as StrictState
import Control.Monad.Trans.State.Strict qualified as LazyState
import Control.Tracer (Tracer, natTracer)
import Data.Aeson (FromJSON, ToJSON)
import Data.Functor (($>))
import Data.Map qualified as Map
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Q2io.Class (
  MonadBlockchain (..),
  MonadBlockchainCardanoNodeT,
 )
import Q2io.CoinSelection (
  BalanceTxError,
  TxBalancingMessage,
 )
import Q2io.CoinSelection qualified
import Q2io.MockChain (MockchainT, utxoSet)
import Q2io.MonadLog (MonadLog, MonadLogIgnoreT)
import Q2io.NodeClient.WaitForTxnClient (MonadBlockchainWaitingT (..))
import Q2io.Utils (liftEither, liftResult)
import Q2io.Utxos (
  BalanceChanges,
  fromApiUtxo,
  fromUtxoTx,
  onlyCredentials,
  toApiUtxo,
 )
import Q2io.Wallet.API qualified as Wallet.API
import Q2io.Wallet.Operator (
  Operator (..),
  Signing,
  operatorPaymentCredential,
  returnOutputFor,
  signTxOperator,
 )
import Servant.Client (ClientEnv)
import Test.QuickCheck.Monadic (PropertyM)

class (Monad m) => MonadUtxoQuery m where
  utxosByPaymentCredentials :: Set PaymentCredential -> m (UTxO BabbageEra)

utxosByPayment :: (MonadUtxoQuery m) => PaymentCredential -> m (UTxO BabbageEra)
utxosByPayment = utxosByPaymentCredentials . Set.singleton

instance (Monad m) => MonadUtxoQuery (MockchainT m) where
  utxosByPaymentCredentials cred = toApiUtxo . onlyCredentials cred <$> utxoSet

instance (MonadUtxoQuery m) => MonadUtxoQuery (ResultT m) where
  utxosByPaymentCredentials = lift . utxosByPaymentCredentials

instance (MonadUtxoQuery m) => MonadUtxoQuery (ExceptT e m) where
  utxosByPaymentCredentials = lift . utxosByPaymentCredentials

instance (MonadUtxoQuery m) => MonadUtxoQuery (ReaderT e m) where
  utxosByPaymentCredentials = lift . utxosByPaymentCredentials

instance (MonadUtxoQuery m) => MonadUtxoQuery (StrictState.StateT s m) where
  utxosByPaymentCredentials = lift . utxosByPaymentCredentials

instance (MonadUtxoQuery m) => MonadUtxoQuery (LazyState.StateT s m) where
  utxosByPaymentCredentials = lift . utxosByPaymentCredentials

instance (MonadUtxoQuery m) => MonadUtxoQuery (MonadBlockchainCardanoNodeT e m) where
  utxosByPaymentCredentials = lift . utxosByPaymentCredentials

instance (MonadUtxoQuery m) => MonadUtxoQuery (MonadLogIgnoreT m) where
  utxosByPaymentCredentials = lift . utxosByPaymentCredentials

instance (MonadUtxoQuery m) => MonadUtxoQuery (PropertyM m) where
  utxosByPaymentCredentials = lift . utxosByPaymentCredentials

deriving newtype instance (MonadUtxoQuery m) => MonadUtxoQuery (MonadBlockchainWaitingT m)

{- | Balance the transaction body using the UTxOs locked by the payment credentials,
returning any unused funds to the given return output
|
-}
balanceTx
  :: (MonadBlockchain m, MonadUtxoQuery m)
  => Tracer m TxBalancingMessage
  -> [PaymentCredential]
  -> TxOut CtxTx BabbageEra
  -> TxBodyContent BuildTx BabbageEra
  -> m (Either BalanceTxError (BalancedTxBody BabbageEra, BalanceChanges))
balanceTx dbg inputCredentials changeOutput txBody = do
  o <- fromApiUtxo <$> utxosByPaymentCredentials (Set.fromList inputCredentials)
  runExceptT (Q2io.CoinSelection.balanceTx (natTracer lift dbg) changeOutput o txBody)

newtype WalletAPIQueryT m a = WalletAPIQueryT {runWalletAPIQueryT_ :: ReaderT ClientEnv m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadBlockchain, MonadLog)

runWalletAPIQueryT :: ClientEnv -> WalletAPIQueryT m a -> m a
runWalletAPIQueryT env (WalletAPIQueryT action) = runReaderT action env

instance (MonadIO m) => MonadUtxoQuery (WalletAPIQueryT m) where
  utxosByPaymentCredentials credentials = WalletAPIQueryT $ do
    result <- ask >>= liftIO . Wallet.API.getUTxOs
    case result of
      Left err -> do
        -- TODO: Better error handling
        let msg = "WalletAPI: Error when calling remote server: " <> show err
        liftIO (putStrLn msg)
        error msg
      Right x -> pure (toApiUtxo $ onlyCredentials credentials $ fromUtxoTx x)

deriving newtype instance (MonadError e m) => MonadError e (WalletAPIQueryT m)

-- | Balance a transaction body using the funds locked by one of a list of payment credentials
balancePaymentCredentials
  :: (MonadBlockchain m, MonadUtxoQuery m, MonadError BalanceAndSubmitError m)
  => Tracer m TxBalancingMessage
  -> C.PaymentCredential
  -- ^ Primary payment credential, used for return output
  -> [C.PaymentCredential]
  -- ^ Other payment credentials, used for balancing
  -> Maybe (C.TxOut C.CtxTx C.BabbageEra)
  -> C.TxBodyContent C.BuildTx C.BabbageEra
  -> m (C.Tx C.BabbageEra)
balancePaymentCredentials dbg primaryCred otherCreds returnOutput txBody = do
  output <- maybe (returnOutputFor primaryCred) pure returnOutput
  (C.BalancedTxBody _ txbody _changeOutput _fee, _) <-
    liftEither BalanceError (balanceTx dbg (primaryCred : otherCreds) output txBody)
  pure (C.makeSignedTransaction [] txbody)

-- | Balance a transaction body using the funds locked by the payment credential
balancePaymentCredential
  :: (MonadBlockchain m, MonadUtxoQuery m, MonadError BalanceAndSubmitError m)
  => Tracer m TxBalancingMessage
  -> C.PaymentCredential
  -> Maybe (C.TxOut C.CtxTx C.BabbageEra)
  -> C.TxBodyContent C.BuildTx C.BabbageEra
  -> m (C.Tx C.BabbageEra)
balancePaymentCredential dbg cred = balancePaymentCredentials dbg cred []

-- | Balance a transaction body, sign it with the operator's key, and submit it to the network.
balanceAndSubmitOperator
  :: (MonadBlockchain m, MonadUtxoQuery m, MonadError BalanceAndSubmitError m)
  => Tracer m TxBalancingMessage
  -> Operator Signing
  -> Maybe (C.TxOut C.CtxTx C.BabbageEra)
  -> C.TxBodyContent C.BuildTx C.BabbageEra
  -> m (C.Tx C.BabbageEra)
balanceAndSubmitOperator dbg op changeOut txBody =
  balancePaymentCredential dbg (operatorPaymentCredential op) changeOut txBody
    >>= signAndSubmitOperator op

-- | Add the operator's signature to the transaction and send it to the blockchain
signAndSubmitOperator
  :: (MonadBlockchain m, MonadError BalanceAndSubmitError m)
  => Operator Signing
  -> C.Tx C.BabbageEra
  -> m (C.Tx C.BabbageEra)
signAndSubmitOperator op tx = do
  let finalTx = signTxOperator op tx
  liftResult SubmitError (sendTx finalTx) $> finalTx

{- | UTxOs that are locked by the operator's payment credential
|
-}
operatorUtxos :: (MonadUtxoQuery m) => Operator k -> m (C.UTxO C.BabbageEra)
operatorUtxos = utxosByPayment . operatorPaymentCredential

-- | Select a single UTxO that is controlled by the operator. |
selectOperatorUTxO
  :: (MonadUtxoQuery m) => Operator k -> m (Maybe (C.TxIn, C.TxOut C.CtxUTxO C.BabbageEra))
selectOperatorUTxO operator = fmap (listToMaybe . Map.toList . C.unUTxO) (operatorUtxos operator)

-- | Failures during txn balancing and submission
data BalanceAndSubmitError
  = BalanceError BalanceTxError
  | SubmitError String
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)
