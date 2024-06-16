{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
-- 1.1.0.0 will be enabled in conway
{-# OPTIONS_GHC -fobject-code -fno-ignore-interface-pragmas -fno-omit-interface-pragmas -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

-- | Scripts used for testing
module Scripts (
  v2SpendingScriptSerialised,
  v2SpendingScript,
  v2StakingScript,
  matchingIndexValidatorScript,
  matchingIndexMPScript,
  spendMatchingIndex,
  mintMatchingIndex,
) where

import Cardano.Api (TxIn)
import Cardano.Api.Shelley qualified as C
import PlutusLedgerApi.Common (SerialisedScript)
import PlutusLedgerApi.Test.Examples (alwaysSucceedingNAryFunction)
import PlutusTx (BuiltinData, CompiledCode)
import PlutusTx qualified
import Q2io.BuildTx (MonadBuildTx)
import Q2io.BuildTx qualified as BuildTx
import Scripts.MatchingIndex qualified as MatchingIndex
import "q2io-base" Q2io.Scripts (
  compiledCodeToScript,
  toHashableScriptData,
 )

v2SpendingScript :: C.PlutusScript C.PlutusScriptV2
v2SpendingScript = C.PlutusScriptSerialised $ alwaysSucceedingNAryFunction 3

v2SpendingScriptSerialised :: SerialisedScript
v2SpendingScriptSerialised = alwaysSucceedingNAryFunction 3

v2StakingScript :: C.PlutusScript C.PlutusScriptV2
v2StakingScript = C.PlutusScriptSerialised $ alwaysSucceedingNAryFunction 2

matchingIndexValidatorCompiled :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
matchingIndexValidatorCompiled = $$(PlutusTx.compile [||\d r c -> MatchingIndex.validator d r c||])

matchingIndexMPCompiled :: CompiledCode (BuiltinData -> BuiltinData -> ())
matchingIndexMPCompiled = $$(PlutusTx.compile [||\r c -> MatchingIndex.mintingPolicy r c||])

{- | Script that passes if the input's index (in the list of transaction inputs)
  matches the number passed as the redeemer
-}
matchingIndexValidatorScript :: C.PlutusScript C.PlutusScriptV2
matchingIndexValidatorScript = compiledCodeToScript matchingIndexValidatorCompiled

matchingIndexMPScript :: C.PlutusScript C.PlutusScriptV2
matchingIndexMPScript = compiledCodeToScript matchingIndexMPCompiled

{- | Spend an output locked by 'matchingIndexValidatorScript', setting
the redeemer to the index of the input in the final transaction
-}
spendMatchingIndex :: (MonadBuildTx m) => TxIn -> m ()
spendMatchingIndex txi =
  let witness txBody =
        C.ScriptWitness C.ScriptWitnessForSpending $
          BuildTx.buildV2ScriptWitness
            matchingIndexValidatorScript
            (C.ScriptDatumForTxIn $ toHashableScriptData ())
            (fromIntegral @Int @Integer $ BuildTx.findIndexSpending txi txBody)
   in BuildTx.setScriptsValid >> BuildTx.addInputWithTxBody txi witness

{- | Mint a token from the 'matchingIndexMPScript', setting
the redeemer to the index of its currency symbol in the final transaction mint
-}
mintMatchingIndex :: (MonadBuildTx m) => C.PolicyId -> C.AssetName -> C.Quantity -> m ()
mintMatchingIndex policy assetName quantity =
  let witness txBody =
        BuildTx.buildV2ScriptWitness
          matchingIndexMPScript
          (C.NoScriptDatumForMint)
          (fromIntegral @Int @Integer $ BuildTx.findIndexMinted policy txBody)
   in BuildTx.setScriptsValid >> BuildTx.addMintWithTxBody policy assetName quantity witness
