module Portfolio where

import Data.List

import Coin
import Action

data Portfolio = Portfolio {
  _equity   :: USD,
  _actions  :: [Action]
} deriving Show

testCoin1 = Coin "BTC" "Bitcoin" 43443.3 [43423.0]
testCoin2 = Coin "ETH" "Etherium" 3393.51 [3392.51]
testAction1 = Action testCoin1 200 43143.2 Owned
testAction2 = Action testCoin2 340 3413.3 Owned
testActions = [testAction1, testAction2]

startPortfolio :: Portfolio
startPortfolio = Portfolio 6000 testActions

portfolioStatus :: Portfolio -> String
portfolioStatus p = do
  let header = "--- Portfolio ---\n"
  let av = "Available: " ++ (show $ available p) ++ " USD\n"
  let al = "Allocated: " ++ (show $ allocated p) ++ " USD\n"
  let ac = actionsStatus $ _actions p
  header ++ av ++ al ++ ac

allocated :: Portfolio -> USD
allocated p = sum $ map _invested (_actions p)

available :: Portfolio -> USD
available p = (_equity p) - allocated p

canBuy :: Portfolio -> Action -> Bool
canBuy p a = (available p) >= (_invested a)

buy :: Portfolio -> Action -> Portfolio
buy p@(Portfolio { _actions = as }) action = case canBuy p action of 
  True -> p { _actions = ([Action.buy action] ++ as) }
  False -> p

findAction :: Portfolio -> String -> Action
findAction p coinName = 
  case (find (\a -> (_abreviation (_coin a)) == coinName) (_actions p)) of
    Just action -> action
    Nothing -> error "coin was not found in portfolio"

sell :: Portfolio -> Action -> Portfolio
sell (Portfolio equity currentActions) actionRequest = do
  let newEquity = equity + (actionProfit actionRequest)
  let newActions = Action.remove currentActions actionRequest
  Portfolio newEquity newActions