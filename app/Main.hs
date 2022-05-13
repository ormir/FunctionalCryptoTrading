module Main where

import Trader

main :: IO ()
main = do
  putStrLn ""
  putStrLn "--------------------"
  putStrLn "--- CryptoTrader ---"
  putStrLn "--------------------"
  putStrLn "Created by Bjorna Kalaja, Ormir Gjurgjej \n\n"
  putStrLn "Wellcome to CryptoTrader!"
  let trader = startTrader
  putStrLn "These are the currenlty available crypto coins"
  putStrLn $ traderStatus trader
  putStrLn "+++ Trader buy new coin +++"
  let trader1 = buy trader "UNI" 100.0
  putStrLn $ traderStatus trader1
  putStrLn "+++ Trader sell ETH +++"
  let trader2 = sell trader1 "ETH"
  putStrLn $ traderStatus trader2