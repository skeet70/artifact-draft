import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Data.Card
import Data.Pack

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" [packTests]

packTests :: TestTree
packTests = testGroup "Pack Tests" [packActionTests]

packActionTests :: TestTree
packActionTests = testGroup "Action Tests" [handlePackEventTests]

testCardList =
  [ (Card "Axe")
  , (Card "Cunning Plan")
  , (Card "Cunning Plan")
  , (Card "Selfish Cleric")
  ]

handlePackEventTests :: TestTree
handlePackEventTests =
  testGroup "Projection Tests" $
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
    handlePackEvent (Pack []) (Initialized testCardList) @?= Pack testCardList
  ]
