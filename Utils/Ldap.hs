module Utils.Ldap where

import Import
import LDAP.Init
import LDAP.Search

data LdapUser = LdapUser { name      :: Text
                         , email     :: Text
                         , active    :: Bool
                         , onFloor   :: Bool
                         , financial :: Bool
                         } deriving (Show)

unknownUser :: LdapUser
unknownUser = LdapUser "Unknown User" "" False False False

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

getUsersLdap :: Text -> IO LdapUser
getUsersLdap uid = do
    print $ "Fetching uid " ++ (unpack uid)
    l <- ldapInitialize "ldaps://ldap.csh.rit.edu"
    ldapSimpleBind l "uid=dgonyeo,ou=users,dc=csh,dc=rit,dc=edu" "lolpassword"
    entries <- ldapSearch
                l
                (Just "dc=csh,dc=rit,dc=edu")
                LdapScopeSubtree
                (Just $ "uid=" ++ (unpack uid))
                (LDAPAttrList ["dn", "cn","mail","active","onfloor"])
                False
    finentries <- ldapSearch
                l
                (Just "cn=Financial,ou=Committees,dc=csh,dc=rit,dc=edu")
                LdapScopeSubtree
                Nothing
                (LDAPAttrList ["head"])
                False
    case (entries,finentries) of
        ([(LDAPEntry _ res)],[(LDAPEntry _ finres)]) -> 
            case ( getValue "dn"      res
                 , getValue "cn"      res
                 , getValue "mail"    res
                 , getValue "active"  res
                 , getValue "onfloor" res
                 ) of
                    (Just dn,Just cn,Just mail,Just act,Just onfloor) ->
                            let tcn   = pack cn
                                tmail = pack mail
                                bact  = act == "1"
                                bonfl = onfloor == "1"
                            in case getValues "head" finres of
                                Just heads -> return $ LdapUser tcn tmail bact bonfl (dn `elem` heads)
                                Nothing -> return $ LdapUser tcn tmail bact bonfl False
                    _ -> return unknownUser
        _ -> return unknownUser
