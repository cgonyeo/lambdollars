{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
module Handler.Swag 
    ( getSwagR
    , postSwagR
    , getSwagAdminR
    ) where

import Import
import Network.HTTP.Types (status200,status201,status404)
import Data.Int

getSwagR :: Handler Html
getSwagR = do
    swags <- map entityVal `fmap` (runDB $ selectList [] [Asc SwagCost, Asc SwagName])
    defaultLayout $ do
        setTitle "Swag"
        $(widgetFile "swag")

data SwagPurchase = SwagPurchase Int64 Int64

instance FromJSON SwagPurchase where
    parseJSON (Object o) = SwagPurchase
                            <$> o .: "uid"
                            <*> o .: "num"

postSwagR :: Handler ()
postSwagR = do
    (SwagPurchase sid num) <- requireJsonBody :: Handler SwagPurchase
    user <- getUser `fmap` waiRequest
    (status,s :: Text) <- runDB $ do
        swag <- get $ SwagKey sid
        case swag of
            Just (Swag _ _ _ _ _ c a) ->
                   if a - num >= 0
                       then do
                           let sale = Sale sid
                                      user
                                      num
                                      (c * (fromIntegral num))
                                      False
                           _ <- insert sale
                           _ <- update (SwagKey sid) [SwagAmount =. (a - num)]
                           return (status201,"SUCCESS")
                       else return (status200,"OUT_OF_INVENTORY")
            Nothing -> return (status404,"SWAG_NOT_FOUND")
    sendResponseStatus status s

getSwagAdminR :: Handler Html
getSwagAdminR = do
    --user <- getUser `fmap` waiRequest
    swags <- map entityVal `fmap` (runDB $ selectList [] [Asc SwagCost, Asc SwagName])
    defaultLayout $ do
        setTitle "Swag Admin"
        $(widgetFile "swagadmin")
