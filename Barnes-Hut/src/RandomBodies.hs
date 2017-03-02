module RandomBodies where

import Test.QuickCheck

getList = find 5 where
  find 0 = return []
  find n = do
    ch <- getChar
    if ch `elem` ['a'..'e'] 
    then do
          t1 <- find (n - 1)
          return (ch : t1)
    else find n

getList' :: IO String
getList' = take5 <$> getContents

take5 :: String -> String
take5 = take 5 . filter (`elem` ['a'..'e'])

deepCheck p = check (qdefaultConfig { configMaxTestq = 10000}) p
