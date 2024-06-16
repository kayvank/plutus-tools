{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Q2io.Scripts where

import Cardano.Api.Shelley as CApi
import Cardano.Ledger.Plutus.Data (Data (..))
import Ouroboros.Consensus.Shelley.Eras (StandardBabbage)
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusLedgerApi.V1 qualified as PV1
import PlutusTx (CompiledCode)

-- | Get the 'PlutusScript' of a 'CompiledCode'
compiledCodeToScript :: CompiledCode a -> PlutusScript lang
compiledCodeToScript = CApi.PlutusScriptSerialised . serialiseCompiledCode

fromScriptData :: (PV1.FromData a) => CApi.ScriptData -> Maybe a
fromScriptData (CApi.toPlutusData -> d) = PV1.fromData d

toScriptData :: (PV1.ToData a) => a -> CApi.ScriptData
toScriptData = CApi.fromPlutusData . PV1.toData

fromHashableScriptData :: (PV1.FromData a) => CApi.HashableScriptData -> Maybe a
fromHashableScriptData (CApi.toPlutusData . CApi.getScriptData -> d) = PV1.fromData d

toHashableScriptData :: (PV1.ToData a) => a -> CApi.HashableScriptData
toHashableScriptData = CApi.fromAlonzoData @StandardBabbage . Data . PV1.toData
