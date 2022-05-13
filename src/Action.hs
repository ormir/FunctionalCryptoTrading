module Action where

import Coin

type USD = Float

data ActionState = RequestBuy | Owned | RequestSell | Sold deriving Show

data Action = Action {
  _coin     :: Coin,
  _invested :: USD,
  _open     :: USD,
  _state    :: ActionState
} deriving Show

actionsStatus :: [Action] -> String
actionsStatus [] = []
actionsStatus (x:xs) = actionStatus x ++ "\n" ++ actionsStatus xs

actionStatus :: Action -> String
actionStatus a = (_abreviation $ _coin a) ++ "\t P/L: " ++ (show $ actionProfit a)

-- action profit %
actionProfit100 :: Action -> Float
actionProfit100 a = (_open a) / (_price (_coin a))

-- action profit USD
actionProfit :: Action -> USD
actionProfit a = (actionProfit100 a) * invested - invested
  where invested = _invested a

buy :: Action -> Action
buy a@(Action { _state = s}) = a { _state = Owned }

remove :: [Action] -> Action -> [Action]
remove as ra = [a | a <- as, (_abreviation (_coin a)) /= (_abreviation (_coin ra))]