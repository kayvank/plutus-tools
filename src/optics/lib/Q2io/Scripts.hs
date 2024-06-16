{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | Functions for dealing with scripts and datums
module Q2io.Scripts (
  -- * FromData / ToData
  fromScriptData,
  toScriptData,
) where

import Cardano.Api.Shelley qualified as C
import PlutusLedgerApi.V1 qualified as PV1

fromScriptData :: (PV1.FromData a) => C.ScriptData -> Maybe a
fromScriptData (C.toPlutusData -> d) = PV1.fromData d

toScriptData :: (PV1.ToData a) => a -> C.ScriptData
toScriptData = C.fromPlutusData . PV1.toData
