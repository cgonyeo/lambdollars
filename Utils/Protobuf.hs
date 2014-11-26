{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module Utils.Protobuf where

import Import
import qualified Data.ProtocolBuffers as P
import qualified Data.Serialize.Get as G
import qualified Data.Serialize.Put as SP
import qualified Data.ByteString.Char8 as C
import qualified Data.Hex as H
import GHC.Generics (Generic)

import Utils.Ldap

data SerLdapUser = SerLdapUser { sername      :: P.Required 1 (P.Value Text)
                               , seremail     :: P.Required 2 (P.Value Text)
                               , seractive    :: P.Required 3 (P.Value Bool)
                               , seronfloor   :: P.Required 4 (P.Value Bool)
                               , serfinancial :: P.Required 5 (P.Value Bool)
                               } deriving (Generic,Show)
instance P.Encode SerLdapUser
instance P.Decode SerLdapUser

luToSlu :: LdapUser -> SerLdapUser
luToSlu (LdapUser n e a o f) =
    SerLdapUser { sername      = P.putField $ n
                , seremail     = P.putField $ e
                , seractive    = P.putField $ a
                , seronfloor   = P.putField $ o
                , serfinancial = P.putField $ f
                }

sluToLu :: SerLdapUser -> LdapUser
sluToLu slu = let n = P.getField $ sername slu
                  e = P.getField $ seremail slu
                  a = P.getField $ seractive slu
                  o = P.getField $ seronfloor slu
                  f = P.getField $ serfinancial slu
              in LdapUser n e a o f

serLdapUser :: LdapUser -> Text
serLdapUser lu = pack $ C.unpack $ fmap H.hex SP.runPut $ P.encodeMessage $ luToSlu lu

desLdapUser :: Text -> Maybe LdapUser
desLdapUser s = let eth = G.runGet P.decodeMessage =<< H.unhex (C.pack $ unpack s)
                in case eth of
                    Left _ -> Nothing
                    Right slu -> Just $ sluToLu slu
