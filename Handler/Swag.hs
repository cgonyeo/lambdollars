{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
module Handler.Swag 
    ( getSwagR
    , postSwagR
    ) where

import Import
import Network.HTTP.Types (status200,status201,status400)
import Data.Text.Read (decimal)

getSwagR :: Handler Html
getSwagR = do
    swagentities :: [Entity Swag] <- runDB $ selectList [] [Asc SwagCost, Asc SwagName]
    let swags = map (entityVal) swagentities
    defaultLayout $ do
        setTitle "Swag"
        $(widgetFile "swag")

postSwagR :: Handler ()
postSwagR = do
    req <- waiRequest
    let user = getUser req
    msid <- lookupPostParam "uid"
    mnum <- lookupPostParam "num"
    case (msid,mnum) of
        (Just tsid,Just tnum) -> do
            let esid = decimal tsid
                enum = decimal tnum
            case (esid,enum) of
                (Right (sid, _), Right (num, _)) -> do
                    s <- runDB $ do swagentity <- getBy $ UniqueSwag sid
                                    case swagentity of
                                        Just (Entity _ (Swag _ _ _ _ _ c a)) ->
                                               if a - num >= 0
                                                   then do let sale = Sale sid
                                                                      user
                                                                      num
                                                                      (c * (fromIntegral num))
                                                                      False
                                                           _ <- insert sale
                                                           _ <- updateWhere [SwagUid ==. sid] [SwagAmount =. (a - num)]
                                                           return True
                                                   else return False
                                        Nothing -> return False
                    if s
                        then sendResponseStatus status201 ("CREATED" :: Text)
                        else sendResponseStatus status200 ("OUT_OF_INVENTORY" :: Text)
                _ -> sendResponseStatus status400 ("BAD_REQUEST2" :: Text)
        _ -> sendResponseStatus status400 ("BAD_REQUEST1" :: Text)
