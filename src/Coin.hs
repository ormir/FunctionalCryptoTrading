module Coin where

data Coin = Coin {
  _abreviation    :: String,
  _name           :: String,
  _price          :: Float,
  _history        :: [Float]
} deriving Show

testCoin :: Coin
testCoin = Coin "BTC" "Bitcoin" 43443.3 [43423.0]

changeStatus :: Coin -> Float
changeStatus c = (_price c) - (last $ _history c)  

statusCoin :: Coin -> String
statusCoin c = (_abreviation c) ++ " \t " ++ (show $ _price c) ++ " \t " ++ (show $ changeStatus c)

statusCoins :: [Coin] -> String
statusCoins [] = []
statusCoins (x:xs) = statusCoin x ++ "\n" ++ statusCoins xs