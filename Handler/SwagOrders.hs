{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
module Handler.SwagOrders where

import Import
import Utils
import Handler.SwagForms

getSwagOrdersR :: Handler Html
getSwagOrdersR = do
    restrictToAdmins

    sales <- map entityVal `fmap` (runDB $ selectList [] [Desc SaleSuid])
    let compsales  = filter (\(Sale _ _ _ _ _ c) -> c) sales
        ncompsales = filter (\(Sale _ _ _ _ _ c) -> not c) sales

    mcswags <- runDB $ mapM (\(Sale uid _ _ _ _ _) -> selectFirst [SwagUid ==. uid ] []) compsales
    let cswags = map (\(Just eswag) -> entityVal eswag) mcswags
    cforms <- mapM (\(Sale _ _ _ n t c) -> generateFormPost $ saveSaleForm n t c) compsales
    let cformsAndIds = zipWith3 (\(Sale _ sid u _ _ _) (Swag _ sname _ _ _ _ _) (fwidg,fenc) -> (u,sname,sid,fwidg,fenc)) compsales cswags cforms

    mncswags <- runDB $ mapM (\(Sale uid _ _ _ _ _) -> selectFirst [SwagUid ==. uid ] []) ncompsales
    let ncswags = map (\(Just eswag) -> entityVal eswag) mncswags
    ncforms <- mapM (\(Sale _ _ _ n t c) -> generateFormPost $ saveSaleForm n t c) ncompsales
    let ncformsAndIds = zipWith3 (\(Sale _ sid u _ _ _) (Swag _ sname _ _ _ _ _) (fwidg,fenc) -> (u,sname,sid,fwidg,fenc)) ncompsales ncswags ncforms

    defaultLayout $ do
        setTitle "Swag Orders"
        $(widgetFile "swagorders")

postSwagOrdersPR :: Int -> Handler Html
postSwagOrdersPR sid = do
    restrictToAdmins
    ((result, widget), enctype) <- runFormPost $ saveSaleForm 0 0 False
    case result of
        FormSuccess (num,total,deleted,comp) -> do
            runDB $ if not deleted
                        then let updates = [ SaleNumpurchased =. num
                                           , SaleTotal =. total
                                           , SaleCompleted =. comp
                                           ]
                             in update (SaleKey $ fromIntegral sid) updates
                        else delete (SaleKey $ fromIntegral sid)
            redirect SwagOrdersR
        _ -> defaultLayout
            [whamlet|
                <p>There was a problem with your input. Please try again.
                <form method=post action=@{SwagOrdersPR $ fromIntegral sid} enctype=#{enctype}>
                    ^{widget}
                    <input type="submit" value="Save">
            |]
