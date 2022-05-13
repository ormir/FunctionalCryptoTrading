module Market where

import Data.List

import Coin

data Market = Market {
  _coins :: [Coin]
} deriving Show

startMarket :: Market
startMarket = Market generateStartingCoins

generateStartingCoins :: [Coin]
generateStartingCoins = do
  let btc = Coin "BTC" "Bitcoin" 43443.3 [43443.4]
  let eth = Coin "ETH" "Etherium" 3393.51 [3392.51]
  let doge = Coin "DOGE" "Dogecoin" 0.18143 [0.160]
  let atom = Coin "ATOM" "Cosmos" 42.1711 [42.15]
  let uni = Coin "UNI" "Uniswap" 17.0898 [17.19]
  [btc, eth, doge, atom, uni]

marketStatus :: Market -> String
marketStatus m = do
  let header1 = "--- Market ---\n"
  let header2 = "Coin \t Price  \t Change\n"
  let status = statusCoins $ _coins m
  header1 ++ header2 ++ status

findCoin :: Market -> String -> Coin
findCoin market coinName = 
  case (find (\c -> (_abreviation c) == coinName) (_coins market)) of
    Just coin -> coin
    Nothing -> error "coin not in market"