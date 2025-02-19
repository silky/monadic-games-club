{-# language DuplicateRecordFields #-}
{-# language TemplateHaskell       #-}
{-# language UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
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

import "base" Control.Monad (when)
import "base" Prelude hiding (init, reverse, id)
import "containers" Data.Map (Map)
import "crem" Crem.BaseMachine (InitialState (..))
import "crem" Crem.Decider (Decider (..), EvolutionResult (..))
import "crem" Crem.Render.RenderableVertices (AllVertices (..), RenderableVertices)
import "crem" Crem.Topology
import "singletons-base" Data.Singletons.Base.TH


data A = A { a :: Int }
data B = B { a :: Int }

f :: B -> Int
f = a


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

data PlayerStack = PlayerStack
  { cards :: [Card]
  }

suit :: Card -> Suit
suit (Flower s) = s
suit (Skull s)  = s

newtype PlayerId = PlayerId Int
  deriving newtype (Eq, Show, Num)

newtype PlayerCount = PlayerCount Int
  deriving newtype (Eq, Show, Num, Ord)

data PlayerState = PlayerState
  { id :: PlayerId
  , playerCount :: PlayerCount
  }
  deriving stock (Show)

init :: PlayerId -> PlayerCount -> PlayerState
init playerId count = PlayerState playerId count

nextPlayer :: PlayerState -> PlayerState
nextPlayer PlayerState{id = PlayerId i, playerCount = PlayerCount count}
  = PlayerState
      { id = PlayerId $ i + 1 `mod` count
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
  { flowers :: Int
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


-- * State

data StateData = StateData
  { bet :: BetData
  , currentPlayer :: PlayData
  , playerStacks :: Map PlayerId PlayerStack
  }

data SkullsState (vertex :: SkullsVertex) where
  SkullsInitialState :: SkullsState 'Initial
  SkullsStartedState :: StateData -> SkullsState 'Started

data GameError
  = TooFewPlayers
  | GameAlreadyStarted
  | GameNotStarted
  | NotYourTurn
  deriving stock (Eq, Show)

-- * Machine

decider
  :: InitialState SkullsState
  -> Decider SkullsTopology Command (Either GameError Event)
decider initialState =
  Decider
    { deciderInitialState = initialState
    , decide = decide
    , evolve = evolve
    }
  where
    decide command state
      = case (state, command) of
          --
          -- ** Starting a game
          (SkullsStartedState _, StartGame _) ->
            Left GameAlreadyStarted

          (_, StartGame initialData)
            | players initialData < PlayerCount 2 -> Left TooFewPlayers
            | otherwise ->
              Right $
                GameStarted
                  initialData
                  ( id $
                      nextPlayer
                        (init (PlayerId 0) $ players initialData)
                  )

          --
          -- ** Playing a card
          (SkullsInitialState, PlayCard _) ->
            Left GameNotStarted

          (SkullsStartedState stateData, PlayCard playData)
            | playerId (currentPlayer stateData) /= playerId playData -> Left NotYourTurn
            | otherwise -> Right $ CardPlayed playData

    evolve = undefined
