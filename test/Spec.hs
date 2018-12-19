import Data.List (last)
import Data.Map ((!))
import Eventful (Projection(..), latestProjection)
import Eventful.UUID (uuidFromInteger)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Data.Card
import Data.Draft as D
import Data.Pack as P

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" [packTests, draftTests]

packTests :: TestTree
packTests = testGroup "Pack Tests" [packEventTests]

packEventTests :: TestTree
packEventTests = testGroup "Event Tests" [handlePackEventTests]

testCardList =
  [ (Card "Axe")
  , (Card "Cunning Plan")
  , (Card "Cunning Plan")
  , (Card "Selfish Cleric")
  ]

handlePackEventTests :: TestTree
handlePackEventTests =
  testGroup "handlePackEvent Tests" $
  [ testCase "Processing Picked removes two cards from the pack." $
    handlePackEvent
      (Pack
         [ (Card "Axe")
         , (Card "Bronze Legionnaire")
         , (Card "Cunning Plan")
         , (Card "Selfish Cleric")
         ])
      (Picked (Card "Axe") (Card "Bronze Legionnaire")) @?=
    Pack [(Card "Cunning Plan"), (Card "Selfish Cleric")]
  , testCase "Processing Transformed removes a card, replacing it with another." $
    handlePackEvent
      (Pack testCardList)
      (Transformed (Card "Cunning Plan") (Card "Drow Ranger")) @?=
    Pack
      [ (Card "Drow Ranger")
      , (Card "Axe")
      , (Card "Cunning Plan")
      , (Card "Selfish Cleric")
      ]
  , testCase "Processing Initialized sets the pack to the specified cards." $
    handlePackEvent (Pack []) (P.Initialized testCardList) @?= Pack testCardList
  ]

draftTests :: TestTree
draftTests = testGroup "Draft Tests" [draftEventTests]

draftEventTests :: TestTree
draftEventTests = testGroup "Event Tests" [handleDraftEventTests]

testUsersList = [uuidFromInteger 20, uuidFromInteger 1, uuidFromInteger 2]

testPacksList =
  [ uuidFromInteger 3
  , uuidFromInteger 4
  , uuidFromInteger 5
  , uuidFromInteger 6
  , uuidFromInteger 7
  , uuidFromInteger 8
  ]

handleDraftEventTests :: TestTree
handleDraftEventTests =
  testGroup "handleDraftEvent Tests" $
  [ testCase "Processing PackPassed moves the pack to the next user." $ do
      let initializedDraft =
            latestProjection
              draftProjection
              [D.Initialized testUsersList testPacksList, RoundIncremented]
          firstUser = head $ draftPositions initializedDraft
          firstPack = head $ ((draftState initializedDraft) ! firstUser)
          secondUser = (draftPositions initializedDraft) !! 1
          events = [PackPassed firstUser firstPack]
          state = draftState $ latestProjection draftProjection events
          secondUsersPack = last (state ! secondUser)
      secondUsersPack @?= firstPack
  -- , testCase
  --     "Processing RoundIncremented removes current packs and distributes new ones." $
  --   handleDraftEvent
  --     (Pack testCardList)
  --     (Transformed (Card "Cunning Plan") (Card "Drow Ranger")) @?=
  --   Pack
  --     [ (Card "Drow Ranger")
  --     , (Card "Axe")
  --     , (Card "Cunning Plan")
  --     , (Card "Selfish Cleric")
  --     ]
  -- , testCase "Processing Initialized sets the pack pool and users." $
  --   handleDraftEvent (Pack []) (D.Initialized testCardList) @?=
  --   Pack testCardList
  ]
