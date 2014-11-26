module Utils.Ldap where

import Import
import LDAP.Init
import LDAP.Search
import Data.Char

data LdapUser = LdapUser { name      :: Text
                         , email     :: Text
                         , active    :: Bool
                         , onFloor   :: Bool
                         , financial :: Bool
                         } deriving (Show)

getValues :: String -> [(String, [String])] -> Maybe [String]
getValues _ [] = Nothing
getValues v ((key, vals):attrs)
    | (length vals) > 0 && v == key = Just vals
    | otherwise                     = getValues v attrs

getValue :: String -> [(String, [String])] -> Maybe String
getValue v attrs = case getValues v attrs of
                        Just (x:_) -> Just x
                        Just [] -> Nothing
                        Nothing -> Nothing

getUsersLdap :: Text -> Handler LdapUser
getUsersLdap uid = do
    (entries,finentries) <- liftBase $ do
                  print $ "Fetching uid " ++ (unpack uid)
                  l <- ldapInitialize "ldaps://ldap.csh.rit.edu"
                  ldapSimpleBind l "uid=dgonyeo,ou=users,dc=csh,dc=rit,dc=edu" "lolpassword"
                  entries <- ldapSearch
                              l
                              (Just "dc=csh,dc=rit,dc=edu")
                              LdapScopeSubtree
                              (Just $ "uid=" ++ (unpack uid))
                              (LDAPAttrList ["cn","mail","active","onfloor"])
                              False
                  finentries <-ldapSearch
                              l
                              (Just "cn=Financial,ou=Committees,dc=csh,dc=rit,dc=edu")
                              LdapScopeSubtree
                              Nothing
                              (LDAPAttrList ["head"])
                              False
                  return (entries,finentries)
    case (entries,finentries) of
        ([(LDAPEntry _ res)],[(LDAPEntry _ finres)]) -> 
            case ( getValue "cn"      res
                 , getValue "mail"    res
                 , getValue "active"  res
                 , getValue "onfloor" res
                 ) of
                    (Just cn,Just mail,Just act,Just onfloor) ->
                            let tcn   = pack cn
                                tmail = pack mail
                                bact  = act == "1"
                                bonfl = onfloor == "1"
                                dn = map toLower $ "uid=" ++ (unpack uid) ++ ",ou=Users,dc=csh,dc=rit,dc=edu"
                            in case getValues "head" finres of
                                       Just heads -> return $ LdapUser tcn tmail bact bonfl (dn `elem` (map (\x -> map toLower x) heads))
                                       Nothing -> return $ LdapUser tcn tmail bact bonfl False
                    _ -> permissionDenied "I had problems loading some of your fields from LDAP. Guess you're not a real person."
        _ -> permissionDenied "I couldn't find you in LDAP. You must not be a real person."
