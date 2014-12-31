{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
module Handler.SwagAdmin
    ( getSwagAdminR
    , postSwagEditR
    , postSwagDelR
    ) where

import Import
import Utils
import Handler.SwagForms
import Data.Hashable

getSwagAdminR :: Handler Html
getSwagAdminR = do
    restrictToAdmins
    swags <- map entityVal `fmap` (runDB $ selectList [] [Asc SwagCost, Asc SwagName])
    forms <- mapM (\(Swag _ n sd ld _ c a) -> generateFormPost $ prefillSwagSaveForm n sd ld c a) swags
    let formsAndIds = zipWith (\(Swag sid _ _ _ _ _ _) (fwidg,fenc) -> (sid,fwidg,fenc)) swags forms
    (newFormWidget,newFormEnctype) <- generateFormPost swagSaveForm
    defaultLayout $ do
        setTitle "Swag Admin"
        $(widgetFile "swagadmin")

postSwagEditR :: Int -> Handler Html
postSwagEditR sid = do
    restrictToAdmins
    ((result, widget), enctype) <- runFormPost $ swagSaveForm
    case result of
        FormSuccess (sname,sd,ld,fi,c,a) -> do
            let fname = hash $ fileName fi
                fnametoks = splitOn "." $ fileName fi
                ext = if length fnametoks > 0
                        then fnametoks !! (length fnametoks - 1)
                        else "jpg"
                filename = "/static/images/" ++ (show fname) ++ "." ++ (unpack ext)
                filepath = "." ++ filename
                filenamet = pack filename
            runDB $ if sid == -1
                        then do lastswags <- (map entityVal . (take 1)) `fmap` selectList [] [Desc SwagUid]
                                case lastswags of
                                    [(Swag sid' _ _ _ _ _ _)] -> do
                                        _ <- insert (Swag (sid'+1) sname sd ld filenamet c a)
                                        return ()
                                    [] -> do
                                        _ <- insert (Swag 0 sname sd ld filenamet c a)
                                        return ()
                                    _ -> error "Math is broken"
                        else let updates = [ SwagName      =. sname
                                           , SwagShortdesc =. sd
                                           , SwagLongdesc  =. ld
                                           , SwagCost      =. c
                                           , SwagAmount    =. a
                                           , SwagImageLoc  =. filenamet
                                           ]
                             in update (SwagKey $ fromIntegral sid) updates
            liftBase $ fileMove fi $ filepath
            redirect SwagAdminR
        _ -> defaultLayout
            [whamlet|
                <p>There was a problem with your input. Please try again.
                <form method=post action=@{SwagEditR $ fromIntegral sid} enctype=#{enctype}>
                    ^{widget}
                    <input type="submit" value="Save">
            |]

postSwagDelR :: Int -> Handler Html
postSwagDelR sid = do
    restrictToAdmins
    runDB $ delete $ SwagKey $ fromIntegral sid
    getSwagAdminR
