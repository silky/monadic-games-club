{-# language DuplicateRecordFields #-}
{-# language TemplateHaskell       #-}
{-# language UndecidableInstances  #-}

-- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wmissing-deriving-strategies
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wunrecognised-pragmas
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wunticked-promoted-constructors
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wunused-type-patterns
{-# OPTIONS_GHC -Wno-unused-type-patterns #-}

module Games.Skull where

{-
  Skulls is a simple betting-style game.

  The simple version is: You have 4 flowers and one skull. You can place
  either of these, face down, and play follows this way until someone decides
  to make a "Bet" for how many cards they can pick up without hitting a Skull
  (starting with their own pile!)

  They win if they do that; or lose if they don't.
-}

import "crem" Crem.BaseMachine (InitialState (..))
import "crem" Crem.Decider (Decider (..), EvolutionResult (..))
import "crem" Crem.Render.RenderableVertices (AllVertices (..), RenderableVertices)
import "crem" Crem.Topology
import "singletons-base" Data.Singletons.Base.TH


-- * Domain

data Suit
  = Red
  | Green
  | Blue
  deriving stock (Eq, Show)

data Card
  = Flower Suit
  | Skull  Suit
  deriving stock (Eq, Show)

suit :: Card -> Suit
suit (Flower s) = s
suit (Skull s)  = s

newtype PlayerId = PlayerId Int
  deriving newtype (Eq, Show, Num)

newtype PlayerCount = PlayerCount Int
  deriving newtype (Eq, Show, Num)

data PlayerState = PlayerState
  { playerId :: PlayerId
  , playerCount :: PlayerCount
  }

init :: PlayerId -> PlayerCount -> PlayerState
init playerId count = PlayerState playerId count

next :: PlayerState -> PlayerState
next PlayerState{playerId = PlayerId i, playerCount = PlayerCount count}
  = PlayerState
      { playerId = PlayerId $ i + 1 `mod` count
      , playerCount = PlayerCount count
      }


-- * Commands and events

data InitialData = InitialData
  { players :: PlayerCount
  }
  deriving stock (Eq, Show)

data PlayData = PlayData
  { playerId :: PlayerId
  , card :: Card
  }
  deriving stock (Eq, Show)

data BetData = BetData
  { playerId :: PlayerId
  , flowers :: Int
  }
  deriving stock (Eq, Show)

data Command
  = StartGame InitialData
  | PlayCard PlayData
  | MakeBet BetData
  deriving stock (Show)

data Event
  = GameStarted InitialData PlayerId
  | CardPlayed  PlayData
  | BetMade     BetData


-- * Topology

$( singletons
    [d|
      data SkullsVertex
        = Initial
        | Started
        deriving stock (Eq, Show, Enum, Bounded)

      skullsTopology :: Topology SkullsVertex
      skullsTopology = Topology [(Initial, [Started])]
      |]
 )

deriving via AllVertices SkullsVertex instance RenderableVertices SkullsVertex
