module Trader where

import Data.List

import Market
import Portfolio
import Action
import Coin

data Trader = Trader {
  _market     :: Market,
  _portfolio  :: Portfolio
} deriving Show

startTrader :: Trader
startTrader = Trader startMarket startPortfolio

traderStatus :: Trader -> String
traderStatus trader = do
  let market = marketStatus $ _market trader
  let portfolio = portfolioStatus $ _portfolio trader
  market ++ "\n" ++ portfolio

buy :: Trader -> String -> USD -> Trader
buy (Trader market portfolio) coinName amount = do
  let coin = findCoin market coinName
  let actionRequest = Action coin amount (_price coin) RequestBuy
  let newPortfolio = Portfolio.buy portfolio actionRequest
  Trader market newPortfolio

sell :: Trader -> String -> Trader
sell (Trader market portfolio) coinName = do
  let action = Portfolio.findAction portfolio coinName
  let newPortfolio = Portfolio.sell portfolio action
  Trader market newPortfolio