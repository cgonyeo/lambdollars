{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
module Handler.Swag 
    ( getSwagR
    , postSwagBuyR
    ) where

import Import
import Utils
import Handler.SwagForms

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
