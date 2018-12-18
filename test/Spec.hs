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
packActionTests =
  testGroup "Action Tests" [packProjectionTests, packValidationTests]

testCardList =
  [ (Card "Axe" 0)
  , (Card "Cunning Plan" 1)
  , (Card "Cunning Plan" 1)
  , (Card "Selfish Cleric" 2)
  ]

packProjectionTests :: TestTree
packProjectionTests =
  testGroup "Projection Tests" $
  [ testCase "Projecting PickA removes two cards from the Snapshot." $
    packProjection
      (PackSnapshot
         [ (Card "Axe" 0)
         , (Card "Bronze Legionnaire" 1)
         , (Card "Cunning Plan" 1)
         , (Card "Selfish Cleric" 2)
         ])
      [PickA (Card "Axe" 0) (Card "Bronze Legionnaire" 1)] @?=
    PackSnapshot [(Card "Cunning Plan" 1), (Card "Selfish Cleric" 2)]
  , testCase "Projecting TransformA removes a card, replacing it with another." $
    packProjection
      (PackSnapshot testCardList)
      [TransformA (Card "Cunning Plan" 1) (Card "Drow Ranger" 5)] @?=
    PackSnapshot
      [ (Card "Drow Ranger" 5)
      , (Card "Axe" 0)
      , (Card "Cunning Plan" 1)
      , (Card "Selfish Cleric" 2)
      ]
  , testCase "Projecting InitializeA sets the pack to the specified cards." $
    packProjection packSeed [(InitializeA testCardList)] @?=
    PackSnapshot testCardList
  ]

packValidationTests :: TestTree
packValidationTests =
  testGroup
    "Validation Tests"
    [ testCase "User command validation creates a valid Pick action." $
      processPackUserCommand
        (PackSnapshot testCardList)
        (Pick (Card "Axe" 0) (Card "Cunning Plan" 1)) @?=
      Just (PickA (Card "Axe" 0) (Card "Cunning Plan" 1))
    , testCase
        "User command validation fails on a Pick action if the cards aren't in the pack." $
      processPackUserCommand
        (PackSnapshot testCardList)
        (Pick (Card "Drow Ranger" 5) (Card "Cunning Plan" 1)) @?=
      Nothing
    , testCase "Server command validation creates a valid Initialize action." $
      processPackServerCommand (PackSnapshot []) (Initialize testCardList) @?=
      Just (InitializeA testCardList)
    , testCase
        "Server command validation fails on an Initialize action if the pack isn't empty." $
      processPackServerCommand (PackSnapshot testCardList) (Initialize []) @?=
      Nothing
    , testCase "Server command validation creates a valid Transform action." $
      processPackServerCommand
        (PackSnapshot testCardList)
        (Transform (Card "Cunning Plan" 1) (Card "Drow Ranger" 5)) @?=
      Just (TransformA (Card "Cunning Plan" 1) (Card "Drow Ranger" 5))
    , testCase
        "Server command validation fails on a Transform action if the card to be transformed isn't in the pack." $
      processPackServerCommand
        (PackSnapshot testCardList)
        (Transform (Card "Drow Ranger" 5) (Card "Axe" 0)) @?=
      Nothing
    ]
