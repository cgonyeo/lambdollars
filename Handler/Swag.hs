{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
module Handler.Swag 
    ( getSwagR
    , postSwagBuyR
    , getSwagAdminR
    , postSwagEditR
    , postSwagDelR
    ) where

import Import
import Utils
import Data.Int
import Data.Hashable
import Yesod.Form.Bootstrap3
    ( withSmallInput )

getSwagR :: Handler Html
getSwagR = do
    swags <- map entityVal `fmap` (runDB $ selectList [] [Asc SwagCost, Asc SwagName])
    forms <- mapM (\_ -> generateFormPost swagBuyForm) swags
    let swagsandforms = zip swags forms
    defaultLayout $ do
        setTitle "Swag"
        $(widgetFile "swag")

postSwagBuyR :: Int -> Handler Html
postSwagBuyR sidi = do
    let sid = fromIntegral sidi
    user <- getUser `fmap` waiRequest
    ((result, widget), enctype) <- runFormPost $ swagBuyForm
    case result of
        FormSuccess (SwagBuy num) -> do
            runDB $ do
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
                                   update (SwagKey sid) [SwagAmount =. (a - num)]
                               else return ()
                    Nothing -> return ()
            getSwagR
        _ -> defaultLayout
            [whamlet|
                <p>There was a problem with your input. Please try again.
                <form method=post action=@{SwagBuyR sidi} enctype=#{enctype}>
                    ^{widget}
                    <input type="submit" value="Buy">
            |]

getSwagAdminR :: Handler Html
getSwagAdminR = do
    --user <- getUser `fmap` waiRequest
    swags <- map entityVal `fmap` (runDB $ selectList [] [Asc SwagCost, Asc SwagName])
    forms <- mapM (\(Swag _ n sd ld _ c a) -> generateFormPost $ prefillSwagSaveForm n sd ld c a) swags
    let formsAndIds = zipWith (\(Swag sid _ _ _ _ _ _) (fwidg,fenc) -> (sid,fwidg,fenc)) swags forms
    (newFormWidget,newFormEnctype) <- generateFormPost swagSaveForm
    defaultLayout $ do
        setTitle "Swag Admin"
        $(widgetFile "swagadmin")

data SwagBuy = SwagBuy Int64

swagBuyForm :: Form SwagBuy
swagBuyForm = renderLambdollarsForm $ SwagBuy
    <$> areq intField (withSmallInput "Amount") Nothing

swagSaveForm :: Form (Text,Text,Text,FileInfo,Double,Int64)
swagSaveForm = renderLambdollarsForm $ (,,,,,)
    <$> areq textField (withSmallInput "Name") Nothing
    <*> areq textField (withSmallInput "Short Description") Nothing
    <*> areq textField (withSmallInput "Long Description") Nothing
    <*> fileAFormReq "Upload an image"
    <*> areq doubleField (withSmallInput "Cost") Nothing
    <*> areq intField (withSmallInput "Amount in Stock") Nothing

prefillSwagSaveForm :: Text -> Text -> Text -> Double -> Int64 -> Form (Text,Text,Text,FileInfo,Double,Int64)
prefillSwagSaveForm n sd ld c a = renderLambdollarsForm $ (,,,,,)
    <$> areq textField (withSmallInput "Name") (Just n)
    <*> areq textField (withSmallInput "Short Description") (Just sd)
    <*> areq textField (withSmallInput "Long Description") (Just ld)
    <*> fileAFormReq "Upload an image"
    <*> areq doubleField (withSmallInput "Cost") (Just c)
    <*> areq intField (withSmallInput "Amount in Stock") (Just a)

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
                <form method=post action=@{SwagEditR $ fromIntegral sid} enctype=#{enctype}>
                    ^{widget}
                    <input type="submit" value="Save">
            |]

postSwagDelR :: Int -> Handler Html
postSwagDelR sid = do
    user <- getUser `fmap` waiRequest
    runDB $ delete $ SwagKey $ fromIntegral sid
    getSwagAdminR
