module Lib
    ( someFunc
    , square   
    , ggT
     ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- / Calculate the square of a number
square
 :: Num a => a -- ^ the number 
 -> a -- ^ the square
square a = a*a


-- / Caluclate ggT
ggT :: Integer -> Integer -> Integer
ggT a b = if a == b
    then a
    else if a < b
      then ggT (b - a) a
      else ggT b (a - b)

