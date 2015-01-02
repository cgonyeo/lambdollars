module Utils.Email where

import Import
import Network.Mail.SMTP
import qualified Data.Text.Lazy as L

sendEmail :: String -> Text -> String -> String -> Text -> Text -> Text -> IO ()
sendEmail server from user pass to sub msg =
        sendMailWithLogin server user pass
                $ simpleMail
                    (Address Nothing from)
                    [(Address Nothing to)]
                    []
                    []
                    sub
                    [plainTextPart $ L.fromStrict msg]
