{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
module Handler.Swag 
    ( getSwagR
    ) where

import Import
import Model
import Yesod.Form.Bootstrap3
    ( BootstrapFormLayout (..), renderBootstrap3, withSmallInput )
import Text.Julius
import Data.Monoid
import Data.Aeson (FromJSON,fromJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Network.HTTP.Types (status201)

getSwagR :: Handler Html
getSwagR = do
    swagentities :: [Entity Swag] <- runDB $ selectList [] []
    let swags = map (entityVal) swagentities
    defaultLayout $ do
        setTitle "Swag"
        $(widgetFile "swag")

data SwagPurchase = SwagPurchase { uid :: Int64
                                 , numpurchased :: Int64
                                 }

instance FromJSON SwagPurchase where
    parseJSON (Object o) = SwagPurchase
        <$> o .: "uid"
        <*> o .: "numpurchased"

postSwagR :: Handler ()
postSwagR = do
    (SwagPurchase id num) <- requireJsonBody :: Handler SwagPurchase
    s <- runDB $ do swagentity <- selectList [swagUid ==. id] []
                    if length swagentity == 1
                        then do let [(Swag i n s l p c a)] = entityVal (swagentity !! 0)
                                    sale = Sale id "bimbotron" num (c * (fromIntegral num)) False
                                if a - num > 0
                                    then do insert sale
                                            return True
                                    else return False
                        else return False
    sendResponseStatus status201 ("CREATED" :: Text)
