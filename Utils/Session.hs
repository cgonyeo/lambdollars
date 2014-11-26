module Utils.Session where

import Import
import Network.Wai (Request,requestHeaders)
import Data.Text.Encoding (decodeUtf8)

import Utils.Protobuf
import Utils.Ldap

getUser :: Request -> Text
getUser r = case muser of
                Just user -> decodeUtf8 user
                Nothing   -> "bimbotron"
    where headers = requestHeaders r
          muser = lookup "X-WEBAUTH-USER" headers

getLdapUser :: Handler LdapUser
getLdapUser = do
    mslu <- lookupSession "user"
    case mslu of
        Just slu -> let mlu = desLdapUser slu
                    in case mlu of
                        Just lu -> return lu
                        Nothing -> do setUltDestCurrent
                                      redirect LdapingR
        Nothing -> do setUltDestCurrent
                      redirect LdapingR

restrictToAdmins :: Handler ()
restrictToAdmins = do
    (LdapUser _ _ _ _ f) <- getLdapUser
    case f of
        True -> return ()
        False -> permissionDenied "Only the Financial Director can see that page"
