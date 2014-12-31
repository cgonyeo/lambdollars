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
    (LdapUser _ _ _ _ fina) <- getLdapUser
    swags <- map entityVal `fmap` (runDB $ selectList [] [Asc SwagCost, Asc SwagName])
    forms <- mapM (\_ -> generateFormPost swagBuyForm) swags
    let swagsandforms = zip swags forms
    defaultLayout $ do
        setTitle "CSH Swag"
        $(widgetFile "swag")

postSwagBuyR :: Int -> Handler Html
postSwagBuyR sidi = do
    let sid = fromIntegral sidi
    user <- getUser `fmap` waiRequest
    (LdapUser cn mail act onfl _) <- getLdapUser
    ((result, widget), enctype) <- runFormPost $ swagBuyForm
    conf <- getExtra
    case result of
        FormSuccess (SwagBuy num) -> do
            msg <- runDB $ do
                swag <- get $ SwagKey sid
                lastsale <- (map entityVal . (take 1)) `fmap` selectList [] [Desc SaleSuid]
                let next_suid = case lastsale of
                                 [(Sale _ suid _ _ _ _)] -> suid + 1
                                 _ ->  0
                case swag of
                    Just (Swag _ n _ _ _ c a) -> do
                           if a - num >= 0 && num > 0
                               then do
                                   let sale = Sale sid
                                              next_suid
                                              user
                                              num
                                              (c * (fromIntegral num))
                                              False
                                   _ <- insert sale
                                   update (SwagKey sid) [SwagAmount =. (a - num)]
                                   let to      = pack $ "financial@csh.rit.edu"
                                       subject = pack $ "Order placed by " ++ (unpack cn) ++ " for " ++ (show num) ++ " of " ++ (unpack n)
                                       message = pack $ (unpack cn) ++ " should be reachable at " ++ (unpack mail) ++ " Active: " ++ (show act) ++ " On Floor: " ++ (show onfl)
                                   liftBase $ sendEmail
                                                (smtpserver conf)
                                                (emailfrom conf)
                                                (emailuser conf)
                                                (emailpassword conf)
                                                to
                                                subject
                                                message
                                   return "Order successful"
                               else return $ "We only have " ++ (show a) ++ " of those in stock. You asked for " ++ (show num) ++ "."
                    Nothing -> return "You seem to have requested nonexistent swag. Please try again, or email financial@csh.rit.edu with what you want."
            setMessage $ toHtml msg
            redirect SwagR
        _ -> defaultLayout
            [whamlet|
                <p>There was a problem with your input. Please try again.
                <form method=post action=@{SwagBuyR sidi} enctype=#{enctype}>
                    ^{widget}
                    <input type="submit" value="Buy">
            |]
