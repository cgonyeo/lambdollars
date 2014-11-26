{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
module Handler.Ldap where

import Import
import Utils

getLdapingR :: Handler Html
getLdapingR = defaultLayout $ do
                setTitle "Loading..."
                $(widgetFile "ldaping")

getLdapedR :: Handler Html
getLdapedR = do
    user <- getUser `fmap` waiRequest
    lu <- liftBase $ getUsersLdap user
    setSession "user" $ serLdapUser lu
    redirectUltDest SwagR
