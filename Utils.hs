module Utils where

import Data.Text
import Data.Maybe
import Network.Wai (Request,requestHeaders)
import Data.Text.Encoding (decodeUtf8)
import Data.List (lookup)

getUser :: Request -> Text
getUser r = case muser of
                Just user -> decodeUtf8 user
                Nothing   -> "bimbotron"
    where headers = requestHeaders r
          muser = lookup "X-WEBAUTH-USER" headers
