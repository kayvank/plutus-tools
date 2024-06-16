{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

{- | Conveniences for balancing transactions and selecting coins
on the mockchain
-}
module Q2io.MockChain.CoinSelection (
  balanceAndSubmit,
  tryBalanceAndSubmit,
  SendTxFailed (..),
  balanceAndSubmitReturn,
  paymentTo,
  payToOperator,
  payToOperator',
) where

import Cardano.Api.Shelley (
  BabbageEra,
  BuildTx,
  TxBodyContent,
  Value,
 )
import Cardano.Api.Shelley qualified as C
import Control.Monad.Except (MonadError)
import Control.Tracer (Tracer)
import Data.Functor (($>))
import Q2io.BuildTx (
  buildTx,
  execBuildTx,
  execBuildTx',
  payToAddress,
  setMinAdaDepositAll,
 )
import Q2io.CardanoApi.Lenses (emptyTxOut)
import Q2io.Class (
  MonadBlockchain (..),
  MonadMockchain,
  SendTxFailed (..),
 )
import Q2io.CoinSelection (BalanceTxError, TxBalancingMessage)
import Q2io.CoinSelection qualified as CoinSelection
import Q2io.MockChain qualified as MockChain
import Q2io.MockChain.Defaults qualified as Defaults
import Q2io.Wallet (Wallet)
import Q2io.Wallet qualified as Wallet
import Q2io.Wallet.Operator (Operator (..), verificationKey)

{- | Balance and submit a transaction using the wallet's UTXOs
on the mockchain, using the default network ID
-}
balanceAndSubmit
  :: (MonadMockchain m, MonadError BalanceTxError m)
  => Tracer m TxBalancingMessage
  -> Wallet
  -> TxBodyContent BuildTx BabbageEra
  -> m (Either SendTxFailed (C.Tx CoinSelection.ERA))
balanceAndSubmit dbg wallet tx = do
  n <- networkId
  let walletAddress = Wallet.addressInEra n wallet
      txOut = emptyTxOut walletAddress
  balanceAndSubmitReturn dbg wallet txOut tx

{- | Balance and submit a transaction using the wallet's UTXOs
on the mockchain, using the default network ID. Fail if the
transaction is not accepted by the node.
-}
tryBalanceAndSubmit
  :: (MonadMockchain m, MonadError BalanceTxError m, MonadFail m)
  => Tracer m TxBalancingMessage
  -> Wallet
  -> TxBodyContent BuildTx BabbageEra
  -> m (C.Tx CoinSelection.ERA)
tryBalanceAndSubmit dbg wallet tx = do
  n <- networkId
  let walletAddress = Wallet.addressInEra n wallet
      txOut = emptyTxOut walletAddress
  balanceAndSubmitReturn dbg wallet txOut tx >>= either (fail . show) pure

{- | Balance and submit a transaction using the given return output and the wallet's UTXOs
on the mockchain, using the default network ID
-}
balanceAndSubmitReturn
  :: (MonadMockchain m, MonadError BalanceTxError m)
  => Tracer m TxBalancingMessage
  -> Wallet
  -> C.TxOut C.CtxTx C.BabbageEra
  -> TxBodyContent BuildTx BabbageEra
  -> m (Either SendTxFailed (C.Tx CoinSelection.ERA))
balanceAndSubmitReturn dbg wallet returnOutput tx = do
  u <- MockChain.walletUtxo wallet
  (tx', _) <- CoinSelection.balanceForWalletReturn dbg wallet u returnOutput tx
  k <- sendTx tx'
  pure (k $> tx')

-- | Pay ten Ada from one wallet to another
paymentTo
  :: (MonadMockchain m, MonadError BalanceTxError m)
  => Wallet
  -> Wallet
  -> m (Either SendTxFailed (C.Tx CoinSelection.ERA))
paymentTo wFrom wTo = do
  let tx =
        buildTx $
          execBuildTx
            (payToAddress (Wallet.addressInEra Defaults.networkId wTo) (C.lovelaceToValue 10_000_000))
  balanceAndSubmit mempty wFrom tx

-- | Pay 100 Ada from one of the seed addresses to an @Operator@
payToOperator
  :: (MonadMockchain m, MonadError BalanceTxError m)
  => Tracer m TxBalancingMessage
  -> Wallet
  -> Operator k
  -> m (Either SendTxFailed (C.Tx C.BabbageEra))
payToOperator dbg wFrom = payToOperator' dbg (C.lovelaceToValue 100_000_000) wFrom

-- | Pay some Ada from one of the seed addresses to an @Operator@
payToOperator'
  :: (MonadMockchain m, MonadError BalanceTxError m)
  => Tracer m TxBalancingMessage
  -> Value
  -> Wallet
  -> Operator k
  -> m (Either SendTxFailed (C.Tx C.BabbageEra))
payToOperator' dbg value wFrom Operator{oPaymentKey} = do
  p <- queryProtocolParameters
  let addr =
        C.makeShelleyAddressInEra
          C.ShelleyBasedEraBabbage
          Defaults.networkId
          (C.PaymentCredentialByKey $ C.verificationKeyHash $ verificationKey oPaymentKey)
          C.NoStakeAddress
      tx = execBuildTx' $ payToAddress addr value >> setMinAdaDepositAll p
  balanceAndSubmit dbg wFrom tx
