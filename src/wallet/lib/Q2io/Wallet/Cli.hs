{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | CLI interface for a wallet
module Q2io.Wallet.Cli (
  runMain,
) where

import Cardano.Api qualified as C
import Control.Concurrent (forkIO)
import Control.Exception (bracket)
import Control.Monad (void)
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except (runExceptT)
import Data.Maybe (fromMaybe)
import Katip qualified as K
import Options.Applicative (
  customExecParser,
  disambiguate,
  helper,
  idm,
  info,
  prefs,
  showHelpOnEmpty,
  showHelpOnError,
 )
import Q2io.MonadLog (
  MonadLog,
  MonadLogKatipT (..),
  logInfo,
  logInfoS,
  logWarnS,
 )
import Q2io.NodeClient.Types (runNodeClient)
import Q2io.NodeQueries (loadConnectInfo)
import Q2io.Utxos (PrettyBalance (..))
import Q2io.Wallet qualified as Wallet
import Q2io.Wallet.API qualified as API
import Q2io.Wallet.Cli.Command (
  CliCommand (..),
  commandParser,
 )
import Q2io.Wallet.Cli.Config (Config (..))
import Q2io.Wallet.NodeClient.BalanceClient qualified as NC
import Q2io.Wallet.Operator (
  OperatorConfigVerification,
  loadOperatorFilesVerification,
  operatorAddress,
  operatorPaymentCredential,
 )
import Q2io.Wallet.WalletState qualified as WalletState
import System.IO (stdout)

runMain :: IO ()
runMain = do
  mainScribe <- K.mkHandleScribe (K.ColorLog True) stdout (K.permitItem K.InfoS) K.V2
  initLogEnv <- K.initLogEnv "wallet" "cli"
  let makeLogEnv = K.registerScribe "stdout-main" mainScribe K.defaultScribeSettings initLogEnv
  bracket makeLogEnv K.closeScribes $ \le -> K.runKatipContextT le () "main" $ runMonadLogKatipT $ do
    command <-
      liftIO
        ( customExecParser
            (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError)
            (info (helper <*> commandParser) idm)
        )
    result <- runExceptT $ do
      case command of
        GenerateWallet -> generateWallet
        GenerateSigningKey{verificationKeyFile, signingKeyFile} -> generateSigningKey verificationKeyFile signingKeyFile
        RunWallet config op port -> runWallet le port config op
        ShowAddress config op -> void (showAddress config op)
    case result of
      Left err -> do
        logWarnS "Error in runMain"
        logWarnS (show err)
      Right () -> pure ()

generateWallet :: (MonadIO m, MonadLog m) => m ()
generateWallet = do
  logInfoS "Generating wallet key..."
  key <- liftIO Wallet.generateWallet
  logInfo (Wallet.privateKey key)

generateSigningKey :: (MonadIO m, MonadLog m) => FilePath -> FilePath -> m ()
generateSigningKey verificationKeyFile signingKeyFile = do
  logInfoS "Generating signing key"
  liftIO $ do
    signingKey <- C.generateSigningKey C.AsPaymentKey
    C.writeFileTextEnvelope (C.File signingKeyFile) Nothing signingKey >>= either (error . show) pure
    C.writeFileTextEnvelope (C.File verificationKeyFile) Nothing (C.getVerificationKey signingKey)
      >>= either (error . show) pure

showAddress
  :: (MonadLog m, MonadError C.InitialLedgerStateError m, MonadIO m)
  => Config
  -> OperatorConfigVerification
  -> m C.LocalNodeConnectInfo
showAddress Config{cardanoNodeConfigFile, cardanoNodeSocket} operatorConfig = do
  op <- liftIO (loadOperatorFilesVerification operatorConfig)
  (info_@C.LocalNodeConnectInfo{C.localNodeNetworkId}, _) <-
    loadConnectInfo cardanoNodeConfigFile cardanoNodeSocket
  logInfo $ "Operator address: " <> C.serialiseToBech32 (operatorAddress localNodeNetworkId op)
  pure info_

runWallet
  :: (MonadLog m, MonadError C.InitialLedgerStateError m, MonadIO m)
  => K.LogEnv
  -> Int
  -> Config
  -> OperatorConfigVerification
  -> m ()
runWallet logEnv port Config{cardanoNodeConfigFile, cardanoNodeSocket, walletFile} operatorConfig = do
  initialState <-
    fromMaybe WalletState.initialWalletState <$> liftIO (WalletState.readFromFile walletFile)
  op <- liftIO (loadOperatorFilesVerification operatorConfig)
  logInfoS $ "Resuming from " <> show (WalletState.chainPoint initialState)
  logInfo (PrettyBalance $ WalletState.utxoSet initialState)
  e <- liftIO (NC.balanceClientEnv walletFile initialState)
  logInfoS $ "Starting wallet server on port " <> show port
  _ <- liftIO $ forkIO (API.startServer (NC.bceState e) port)
  let client _ env = do
        pure (NC.balanceClient logEnv "wallet" e initialState (operatorPaymentCredential op) env)
  result <- liftIO $ runExceptT (runNodeClient cardanoNodeConfigFile cardanoNodeSocket client)
  case result of
    Left err -> throwError err
    Right () -> pure ()
