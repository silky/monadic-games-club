{-# language DuplicateRecordFields #-}
{-# language OverloadedRecordDot   #-}

module Game.SkullsSpec where

import "crem" Crem.StateMachine (StateMachineT (..), run)
import "base" Data.Functor.Identity (runIdentity)
import "crem" Crem.BaseMachine (InitialState (..))
import "crem" Crem.Decider (Decider, deciderMachine)
import "hspec" Test.Hspec (Spec, describe, it, shouldBe)
import "monadic-games-club" Game.Skulls.Model

type SkullsGame = Decider SkullsTopology Command (Either GameError (Event, StateBlob))

spec :: Spec
spec = describe "Skulls" $ do
  it "can start with the right number of players" $ do
    let d0 :: SkullsGame
        d0 = decider (InitialState SkullsInitialState)

        expectedEvent :: Event
        expectedEvent
          = GameStarted $ InitialData
              { players = PlayerCount 3, startingPlayer = firstPlayer }

        expectedState :: StateBlob
        expectedState = BlobInitialState

        r = foldl
              (\b a -> run (runIdentity (snd <$> b)) a)
              (pure (Left undefined, Basic $ deciderMachine d0))
              gameSequence

     in runIdentity (fst <$> r) `shouldBe` Right (expectedEvent, expectedState)

gameSequence :: [Command]
gameSequence =
  [ StartGame $
      InitialData
        { players = PlayerCount 3
        , startingPlayer = firstPlayer
        }
  , PlayCard $
      TurnData
        { playerId = firstPlayer
        , card = Flower Red
        }
  , PlayCard $
      TurnData
        { playerId = firstPlayer
        -- { playerId = nextPlayer (PlayerCount 3) firstPlayer
        , card = Flower Blue
        }
  -- , PlayCard $
  --     TurnData
  --       { playerId = nextPlayer (PlayerCount 3) (nextPlayer (PlayerCount 3) firstPlayer)
  --       , card = Flower Orange
  --       }
  ]
