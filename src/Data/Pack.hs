module Data.Pack where

import Eventful

import Data.Card
import Data.List (delete)

data Pack = Pack
  { packCards :: [Card]
  } deriving (Eq, Show)

data PackEvent
  = Picked Card
           Card
  | Transformed Card
                Card
  | Initialized [Card]
  deriving (Eq, Show)

handlePackEvent :: Pack -> PackEvent -> Pack
handlePackEvent _ (Initialized cards) = Pack cards
handlePackEvent (Pack cards) (Picked c1 c2) = Pack (delete c2 $ delete c1 cards)
handlePackEvent (Pack cards) (Transformed c1 c2) = Pack (c2 : delete c1 cards)

packProjection :: Projection Pack PackEvent
packProjection =
  Projection
    {projectionSeed = Pack [], projectionEventHandler = handlePackEvent}
