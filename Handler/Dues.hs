{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
module Handler.Dues where

import Import
import Utils

getDuesR :: Handler Html
getDuesR = do
    user <- getUser `fmap` waiRequest
    (LdapUser cn _ act onfl fina) <- getLdapUser
    currsem <- liftBase $ getCurrSem
    mfalldues <- let syear = case currsem of
                                 Spring year -> year - 1
                                 Summer year -> year
                                 Fall   year -> year
                 in runDB $ selectFirst [ DuesUsername ==. user
                                        , DuesYear     ==. (fromIntegral syear)
                                        , DuesSpring   ==. False] []
    mspringdues <- let syear = case currsem of
                                   Spring year -> year
                                   Summer year -> year + 1
                                   Fall   year -> year + 1
                   in runDB $ selectFirst [ DuesUsername ==. user
                                          , DuesYear     ==. (fromIntegral syear)
                                          , DuesSpring   ==. True] []
    defaultLayout $ do
        setTitle "CSH Dues"
        $(widgetFile "dues")

amountDue :: Bool -> Double
amountDue False = 75
amountDue True  = 80
