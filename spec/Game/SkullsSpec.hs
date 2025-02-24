{-# language DuplicateRecordFields #-}
{-# language OverloadedRecordDot   #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Game.SkullsSpec where

import "QuickCheck" Test.QuickCheck
import "base" Data.Function ((&))
import "base" Data.Functor.Identity (runIdentity, Identity)
import "base" Data.List qualified as List
import "containers" Data.Map (Map)
import "containers" Data.Map qualified as Map
import "crem" Crem.StateMachine (StateMachineT (..), run)
import "hspec" Test.Hspec (Spec, describe, it, shouldBe)
import "monadic-games-club" Game.Skulls.Model

spec :: Spec
spec = describe "Skulls" $ do
  it "can apply picking up rules correct" $ do
    property $ prop_something


newtype Players = Players { unPlayers :: [PlayerId] }
  deriving stock (Show)

instance Arbitrary Players where
  arbitrary = do
    n <- oneof (map pure [ 2 .. 6 ])
    pure $ Players $ map PlayerId [ 0 .. n - 1 ]

newtype PlayerStacks = PlayerStacks { unPlayerStacks :: Map PlayerId [Card] }
  deriving stock (Show)

mkFlexList :: Gen a -> Gen [a]
mkFlexList a = sized $ \n ->
  frequency
    [ (1, return [])
    , (n, (:) <$> a <*> mkFlexList a)
    ]

flexList :: Arbitrary a => Gen [a]
flexList = mkFlexList arbitrary

instance Arbitrary Card where
  arbitrary = frequency [(1, pure Skull), (4, pure Flower)]

genCards :: Gen [Card]
genCards = do
  flexList `suchThat` \x ->
    (length $ filter isSkull x) <= 1
    && (length $ filter isFlower x) <= maxFlowers

genPlayerStacks :: [PlayerId] -> Gen PlayerStacks
genPlayerStacks players = do
  cardsForPlayers <- flip mapM players (\p -> genCards >>= pure . (,) p)
  pure $ PlayerStacks $ Map.fromList cardsForPlayers

instance Arbitrary StateData where
  arbitrary = do
    Players players <- arbitrary @Players
    let current = head players
    PlayerStacks stacks <- genPlayerStacks players
    pure $ StateData
      { sdCurrentPlayer = current
      , sdPlayerStacks  = stacks
      , sdWinCounts     = mempty -- TODO: Later
      , sdLossCounts    = mempty -- TODO: Later
      , sdTotalPlayers  = PlayerCount (length players)
      }

  -- it "can start with the right number of players" $ do
  --   let expectedEvent :: Event
  --       expectedEvent
  --         = GameStarted $ InitialData
  --             { idPlayers = PlayerCount 3, idStartingPlayer = firstPlayer }

  --       expectedState :: StateBlob
  --       expectedState = BlobInitialState

  --       r :: (GameResult, StateMachineT Identity Command GameResult)
  --       r = foldl
  --             (\(_, machine) command -> runIdentity (run machine command))
  --             (GameResult $ undefined, Basic skullsMachine)
  --             gameSequence

  --    in fst r `shouldBe` (GameResult $ Right (expectedEvent, expectedState))

prop_something :: Int -> StateData -> Property
prop_something nFlowers s@StateData{sdCurrentPlayer, sdPlayerStacks} =
  nFlowers > 0 ==>
  tabulate "Outcomes" [show r] go
    & counterexample ("PickUp Result: " <> show r)
    & counterexample ("Expected: " <> show expected)
    & counterexample ("Skull Index: " <> show skullIndex)
    & counterexample ("Stack Count: " <> show stackCount)
    where
      r = fst $ attemptPickupFrom sdCurrentPlayer nFlowers s

      -- For the last case;
      skullIndex = maybe Nothing (List.findIndex (== Skull))
                        $ Map.lookup sdCurrentPlayer sdPlayerStacks
      stackCount = countStackOfPlayer sdCurrentPlayer s
      expected
          = case skullIndex of
              Nothing -> if stackCount >= nFlowers
                            then WonRound
                            else PickedUp stackCount
              Just ix -> if nFlowers > ix
                            then FailedBet
                            else WonRound
      go
        -- * Bluffing
        --
        -- If someone calls your bluff, you loose.
        | playedSkull sdCurrentPlayer s
          && nFlowers >= countStackOfPlayer sdCurrentPlayer s = r == FailedBet
        --
        -- * Easy win
        --
        -- If you only needed to pick up cards from yourself, and you never
        -- played a skull, it's win.
        | not (playedSkull sdCurrentPlayer s)
          && nFlowers <= countStackOfPlayer sdCurrentPlayer s = r == WonRound
        --
        | otherwise = r == expected

gameSequence :: [Command]
gameSequence =
  [ StartGame $
      InitialData
        { idPlayers = PlayerCount 3
        , idStartingPlayer = firstPlayer
        }
  , PlayCard $
      TurnData
        { tdPlayerId = firstPlayer
        , tdCard = Flower
        }
  , PlayCard $
      TurnData
        -- { tdPlayerId = firstPlayer
        { tdPlayerId = nextPlayer (PlayerCount 3) firstPlayer
        , tdCard = Flower
        }
  -- , PlayCard $
  --     TurnData
  --       { playerId = nextPlayer (PlayerCount 3) (nextPlayer (PlayerCount 3) firstPlayer)
  --       , card = Flower Orange
  --       }
  ]
