module Data.Pool where

import Data.Cards (Card)
import Eventful (UUID)

data Pool = Pool
  { poolCards :: [Card]
  , poolUser :: UUID
  } deriving (Show, Eq)
