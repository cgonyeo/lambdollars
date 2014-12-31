module Utils.Calendar where

import Import
import Data.Time.Clock
import Data.Time.Calendar

data Semester = Fall Integer | Spring Integer | Summer Integer deriving(Show,Eq)
 
date :: IO (Integer,Int,Int) -- :: (year,month,day)
date = getCurrentTime >>= return . toGregorian . utctDay

getCurrSem :: IO Semester
getCurrSem = do (year,month,day) <- date
                return $ case () of
                             _ | 1 <= month && month <= 5  -> Spring year
                               | 6 <= month && month <= 8  -> Summer year
                               | 9 <= month && month <= 12 -> Fall   year
