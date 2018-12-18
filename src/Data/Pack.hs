module Data.Pack where

import Data.Card
import Data.List (delete)

data Pack = Pack
  { packId :: Int
  } deriving (Eq, Show)

data PackSnapshot = PackSnapshot
  { packSnapshotCards :: [Card]
  } deriving (Eq, Show)

data PackEvent = PackEvent
  { packEventId :: Int
  , packEventIndex :: Int
  , packEventAction :: PackAction
  , packEventPack :: Int
  } deriving (Eq, Show)

data PackAction
  = PickA Card
          Card
  | TransformA Card
               Card
  | InitializeA [Card]
  deriving (Eq, Show)

data PackServerCommand
  = Initialize [Card]
  | Transform Card
              Card
  deriving (Eq, Show)

data PackUserCommand =
  Pick Card
       Card
  deriving (Eq, Show)

packSeed :: PackSnapshot
packSeed = PackSnapshot []

packProjection :: PackSnapshot -> [PackAction] -> PackSnapshot
packProjection snap [] = snap
packProjection _ ((InitializeA cards):actions) =
  packProjection (PackSnapshot cards) actions
packProjection (PackSnapshot cards) ((PickA c1 c2):actions) =
  packProjection (PackSnapshot (delete c2 $ delete c1 cards)) actions
packProjection (PackSnapshot cards) ((TransformA c1 c2):actions) =
  packProjection (PackSnapshot (c2 : delete c1 cards)) actions

processPackServerCommand ::
     PackSnapshot -> PackServerCommand -> Maybe PackAction
processPackServerCommand (PackSnapshot []) (Initialize cards) =
  Just $ InitializeA cards
processPackServerCommand (PackSnapshot cards) (Transform c1 c2)
  | c1 `elem` cards = Just $ TransformA c1 c2
  | otherwise = Nothing
processPackServerCommand _ _ = Nothing

-- this likely needs the user's pool information too, to ensure they haven't already picked a hero this round.
processPackUserCommand :: PackSnapshot -> PackUserCommand -> Maybe PackAction
processPackUserCommand (PackSnapshot cards) (Pick c1 c2)
  | (c1 `elem` cards) && (c2 `elem` cards) = Just $ PickA c1 c2
  | otherwise = Nothing
