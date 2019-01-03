module Data.Draft where

import Data.Char (ord)
import Data.List (delete, findIndex, splitAt)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Text (unpack)
import qualified Data.Vector as V (fromList, toList)
import Eventful
import System.Random (StdGen, mkStdGen)
import qualified VectorShuffling.Immutable as VS (shuffle)

type UserUUID = UUID

type PackUUID = UUID

data Draft = Draft
  { draftState :: Map.Map UserUUID [PackUUID]
  , draftPositions :: [UserUUID]
  , draftPackPool :: [PackUUID]
  , draftRound :: Int
  } deriving (Eq, Show)

data DraftEvent
  = PackPassed UserUUID
               PackUUID
  | RoundIncremented
  | Initialized [UserUUID]
                [PackUUID]
  deriving (Eq, Show)

-- assuming everything exists because of validation
handleDraftEvent :: Draft -> DraftEvent -> Draft
handleDraftEvent _ (Initialized users packs) =
  Draft (Map.fromSet (\_ -> []) (Set.fromList users)) users' packs' 0
  where
    (users', packs') = shuffle users packs
handleDraftEvent (Draft state positions packs round) RoundIncremented =
  Draft state' positions packs' (round + 1)
  where
    state' = Map.fromList $ zip positions (map (: []) packs)
    packs' = drop (length positions - 1) packs
handleDraftEvent (Draft state positions packs round) (PackPassed user pack) =
  Draft movedPack positions packs round
  where
    position = fromJust $ findIndex (user ==) positions
    nextUser =
      if position == (length positions - 1)
        then positions !! 0
        else positions !! (position + 1)
    movedPack =
      Map.adjust
        (\ps -> ps ++ [pack])
        nextUser
        (Map.adjust (\ps -> delete pack ps) user state)

draftProjection :: Projection Draft DraftEvent
draftProjection =
  Projection
    { projectionSeed =
        Draft
          { draftState = Map.empty
          , draftPositions = []
          , draftPackPool = []
          , draftRound = 0
          }
    , projectionEventHandler = handleDraftEvent
    }

-- shuffle users and packs predictably using the UUIDs as a seed
shuffle :: [UserUUID] -> [PackUUID] -> ([UserUUID], [PackUUID])
shuffle users packs =
  ( (V.toList . fst $ VS.shuffle (V.fromList users) seed)
  , (V.toList . fst $ VS.shuffle (V.fromList packs) seed))
  where
    seed = genSeed users packs

genSeed :: [UserUUID] -> [PackUUID] -> StdGen
genSeed users packs =
  mkStdGen (sum . concat $ map uuidToInts users ++ map uuidToInts packs)
  where
    uuidToInts :: UUID -> [Int]
    uuidToInts uuid = map ord ((unpack (uuidToText uuid)))
