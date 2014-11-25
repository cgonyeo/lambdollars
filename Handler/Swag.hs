{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
module Handler.Swag 
    ( getSwagR
    , postSwagR
    , getSwagAdminR
    , postSwagEditR
    ) where

import Import
import Utils
import Network.HTTP.Types (status200,status201,status404)
import Data.Int
import Data.Hashable
import Control.Monad
import Yesod.Form.Bootstrap3
    ( withSmallInput )

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
    parseJSON _ = mzero

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
    forms <- mapM (\_ -> generateFormPost swagSaveForm) swags
    let formsAndIds = zipWith (\(Swag sid _ _ _ _ _ _) (fwidg,fenc) -> (sid,fwidg,fenc)) swags forms
    (newFormWidget,newFormEnctype) <- generateFormPost swagSaveForm
    defaultLayout $ do
        setTitle "Swag Admin"
        $(widgetFile "swagadmin")

swagSaveForm :: Form (Text,Text,Text,FileInfo,Double,Int64)
swagSaveForm = renderLambdollarsForm $ (,,,,,)
    <$> areq textField (withSmallInput "Name") Nothing
    <*> areq textField (withSmallInput "Short Description") Nothing
    <*> areq textField (withSmallInput "Long Description") Nothing
    <*> fileAFormReq "Upload an image"
    <*> areq doubleField (withSmallInput "Cost") Nothing
    <*> areq intField (withSmallInput "Amount in Stock") Nothing

postSwagEditR :: Int -> Handler Html
postSwagEditR sid = do
    ((result, widget), enctype) <- runFormPost $ swagSaveForm
    case result of
        FormSuccess (name,sd,ld,fi,c,a) -> do
            let fname = hash $ fileName fi
                fnametoks = splitOn "." $ fileName fi
                ext = if length fnametoks > 0
                        then fnametoks !! (length fnametoks - 1)
                        else "jpg"
                filename = "/static/images/" ++ (show fname) ++ "." ++ (unpack ext)
                filepath = "." ++ filename
                filenamet = pack filename
            runDB $ if sid == -1
                        then do [(Swag sid' _ _ _ _ _ _)] <- (map entityVal . (take 1)) `fmap` selectList [] [Desc SwagUid]
                                _ <- insert (Swag (sid'+1) name sd ld filenamet c a)
                                return ()
                        else let updates = [ SwagName      =. name
                                           , SwagShortdesc =. sd
                                           , SwagLongdesc  =. ld
                                           , SwagCost      =. c
                                           , SwagAmount    =. a
                                           , SwagImageLoc  =. filenamet
                                           ]
                             in update (SwagKey $ fromIntegral sid) updates
            liftBase $ fileMove fi $ filepath
            getSwagAdminR
        _ -> defaultLayout
            [whamlet|
                <p>There was a problem with your input. Please try again.
                <form method=post action=@{SwagAdminR} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
            |]
