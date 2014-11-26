{-# LANGUAGE OverloadedStrings #-}
module Utils.Email where

import Import
import Network.Mail.SMTP
import qualified Data.Text.Lazy as L

sendEmail :: Text -> Text -> Text -> IO ()
sendEmail to sub msg = sendMailWithLogin
                            "mail.csh.rit.edu"
                            "dgonyeo"
                            "lolpassword"
                                $ simpleMail
                                    (Address Nothing "dgonyeo@csh.rit.edu")
                                    [(Address Nothing to)]
                                    []
                                    []
                                    sub
                                    [plainTextPart $ L.fromStrict msg]
