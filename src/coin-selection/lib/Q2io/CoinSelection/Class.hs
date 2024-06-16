{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | An effect for balancing transactions
module Q2io.CoinSelection.Class (
  MonadBalance (..),
  BalancingT (..),

  -- * Tracing
  TracingBalancingT (..),
  runTracingBalancingT,
) where

import Cardano.Api.Shelley (
  AddressInEra,
  BabbageEra,
  BuildTx,
  TxBodyContent,
 )
import Cardano.Api.Shelley qualified as C
import Control.Monad.Catch (
  MonadCatch,
  MonadMask,
  MonadThrow,
 )
import Control.Monad.Except (
  ExceptT,
  MonadError,
  runExceptT,
 )
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.State qualified as StrictState
import Control.Monad.Trans.State.Strict qualified as LazyState
import Control.Tracer (Tracer, natTracer)
import Q2io.CardanoApi.Lenses (emptyTxOut)
import Q2io.Class (
  MonadBlockchain (..),
  MonadMockchain (..),
 )
import Q2io.CoinSelection (
  BalanceTxError,
  TxBalancingMessage,
 )
import Q2io.CoinSelection qualified
import Q2io.MonadLog (MonadLog, MonadLogIgnoreT)
import Q2io.Query (MonadUtxoQuery (utxosByPaymentCredentials))
import Q2io.Utxos (
  BalanceChanges (..),
  UtxoSet (..),
 )

{- Note [Transaction Balancing]

Why do we need an extra class for balancing when
we could just use 'Q2io.CoinSelection.balanceTx'?

The reason is that we can use this class to inject modifications to the unbalanced tx,
which is the way we simulate attacks in the testing framework. So for normal operations
we do indeed just call 'Q2io.CoinSelection.balanceTx', but for

-}

-- | Balancing a transaction
class (Monad m) => MonadBalance m where
  -- | Balance the transaction using the given UTXOs and return address.
  balanceTx
    :: AddressInEra BabbageEra
    -- ^ Address used for leftover funds
    -> UtxoSet C.CtxUTxO a
    -- ^ Set of UTxOs that can be used to supply missing funds
    -> TxBodyContent BuildTx BabbageEra
    -- ^ The unbalanced transaction body
    -> m (Either BalanceTxError (C.BalancedTxBody BabbageEra, BalanceChanges))
    -- ^ The balanced transaction body and the balance changes (per address)

newtype BalancingT m a = BalancingT {runBalancingT :: m a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadCatch
    , MonadFail
    , MonadLog
    , MonadThrow
    , MonadMask
    , MonadBlockchain
    )

instance MonadTrans BalancingT where
  lift = BalancingT

deriving newtype instance (MonadError e m) => MonadError e (BalancingT m)

instance (MonadBalance m) => MonadBalance (ExceptT e m) where
  balanceTx addr utxos = lift . balanceTx addr utxos

instance (MonadBalance m) => MonadBalance (ReaderT e m) where
  balanceTx addr utxos = lift . balanceTx addr utxos

instance (MonadBalance m) => MonadBalance (StrictState.StateT s m) where
  balanceTx addr utxos = lift . balanceTx addr utxos

instance (MonadBalance m) => MonadBalance (LazyState.StateT s m) where
  balanceTx addr utxos = lift . balanceTx addr utxos

instance (MonadBalance m) => MonadBalance (MonadLogIgnoreT m) where
  balanceTx addr utxos = lift . balanceTx addr utxos

instance (MonadBlockchain m) => MonadBalance (BalancingT m) where
  balanceTx addr utxos txb = runExceptT (Q2io.CoinSelection.balanceTx mempty (emptyTxOut addr) utxos txb)

instance (MonadMockchain m) => MonadMockchain (BalancingT m) where
  modifySlot = lift . modifySlot
  modifyUtxo = lift . modifyUtxo
  resolveDatumHash = lift . resolveDatumHash

instance (MonadUtxoQuery m) => MonadUtxoQuery (BalancingT m) where
  utxosByPaymentCredentials = lift . utxosByPaymentCredentials

-- | Implementation of @MonadBalance@ that uses the provided tracer for debugging output
newtype TracingBalancingT m a = TracingBalancingT {runTracingBalancingT' :: ReaderT (Tracer m TxBalancingMessage) m a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadCatch
    , MonadFail
    , MonadLog
    , MonadThrow
    , MonadMask
    , MonadBlockchain
    )

instance MonadTrans TracingBalancingT where
  lift = TracingBalancingT . lift

deriving newtype instance (MonadError e m) => MonadError e (TracingBalancingT m)

instance (MonadBlockchain m) => MonadBalance (TracingBalancingT m) where
  balanceTx addr utxos txb = TracingBalancingT $ do
    tr <- ask
    runExceptT (Q2io.CoinSelection.balanceTx (natTracer (lift . lift) tr) (emptyTxOut addr) utxos txb)

instance (MonadMockchain m) => MonadMockchain (TracingBalancingT m) where
  modifySlot = lift . modifySlot
  modifyUtxo = lift . modifyUtxo
  resolveDatumHash = lift . resolveDatumHash

instance (MonadUtxoQuery m) => MonadUtxoQuery (TracingBalancingT m) where
  utxosByPaymentCredentials = lift . utxosByPaymentCredentials

runTracingBalancingT :: Tracer m TxBalancingMessage -> TracingBalancingT m a -> m a
runTracingBalancingT tracer (TracingBalancingT action) = runReaderT action tracer
