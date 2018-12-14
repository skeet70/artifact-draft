module Data.Card where

import Data.Text (Text)

data Card = Card
  { cardName :: Text
  , cardId :: Int
  } deriving (Eq, Show)
