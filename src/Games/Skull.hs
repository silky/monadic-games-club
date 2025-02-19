{-# language DuplicateRecordFields #-}
{-# language OverloadedRecordDot   #-}
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

import "base" Prelude hiding (init, reverse, id)
import "containers" Data.Map (Map, insertWith)
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

newtype PlayerId = PlayerId Int
  deriving newtype (Eq, Show, Num, Ord)

newtype PlayerCount = PlayerCount Int
  deriving newtype (Eq, Show, Num, Ord)

nextPlayer :: PlayerCount -> PlayerId -> PlayerId
nextPlayer (PlayerCount count) (PlayerId i)
  = PlayerId $ i + 1 `mod` count

-- * Commands and events

-- | Data to kick off a game.
data InitialData = InitialData
  { players :: PlayerCount
  , startingPlayer :: PlayerId
  }
  deriving stock (Eq, Show)

-- | This is what happens in a turn.
data TurnData = TurnData
  { playerId :: PlayerId
  , card :: Card
  }
  deriving stock (Eq, Show)

-- TODO: Model someone being allowed to skip making a bet.
data BetData = BetData
  { flowers :: Int
  , playerId :: PlayerId
  }
  deriving stock (Eq, Show)

data Command
  = StartGame InitialData
  | PlayCard TurnData
  | MakeBet BetData
  deriving stock (Show)

data Event
  = GameStarted InitialData
  | CardPlayed  TurnData
  | BetMade     BetData


-- * Topology

$( singletons
    [d|
      data SkullsVertex
        = Initial
        | PlacingCards
        | Betting
        | Finished
        deriving stock (Eq, Show, Enum, Bounded)

      skullsTopology :: Topology SkullsVertex
      skullsTopology = Topology [ (Initial,      [PlacingCards])
                                , (PlacingCards, [Betting])
                                , (Betting,      [Betting, PlacingCards, Finished])
                                ]
      |]
 )

deriving via AllVertices SkullsVertex instance RenderableVertices SkullsVertex


-- * State

data StateData = StateData
  { currentPlayer :: PlayerId
  , playerStacks  :: Map PlayerId [Card]
  , winCounts     :: Map PlayerId Int
  , totalPlayers  :: PlayerCount
  }


data SkullsState (vertex :: SkullsVertex) where
  SkullsInitialState      :: SkullsState Initial
  SkullsPlacingCardsState :: StateData -> SkullsState PlacingCards
  SkullsBettingSate       :: BetData -> StateData -> SkullsState Betting
  SkullsFinishedState     :: StateData -> SkullsState Finished


data GameError
  = TooFewPlayers
  | GameAlreadyStarted
  | GameNotStarted
  | NotYourTurn
  | CantPlaceWhileBetting
  | GameIsOver
  | BetMustBeHigher
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
  -- | Decides the details of which events can trigger state transitions.
  decide :: Command -> SkullsState vertex -> Either GameError Event
  decide command state
    = case (state, command) of
        -- ** Starting a game
        (SkullsInitialState, StartGame initialData)
          | players initialData < PlayerCount 2 -> Left TooFewPlayers
          | otherwise ->
              Right $
                GameStarted
                  initialData

        (SkullsInitialState,         _)           -> Left GameNotStarted
        (SkullsPlacingCardsState {}, StartGame _) -> Left GameAlreadyStarted
        (SkullsBettingSate {},       StartGame _) -> Left GameAlreadyStarted

        -- ** Playing a card
        (SkullsPlacingCardsState stateData, PlayCard turnData)
          | stateData.currentPlayer /= turnData.playerId -> Left NotYourTurn
          | otherwise -> Right $ CardPlayed turnData

        (SkullsBettingSate {}, PlayCard _) -> Left CantPlaceWhileBetting


        -- ** Making a bet
        (SkullsPlacingCardsState _, MakeBet betData) ->
          Right $ BetMade betData

        (SkullsBettingSate existingBet _, MakeBet newBet)
          | flowers newBet > flowers existingBet -> Right $ BetMade newBet
          | otherwise -> Left BetMustBeHigher


        -- ** Bookkeeping
        (SkullsFinishedState {}, _) -> Left GameIsOver

  -- | Perfoms state transitions.
  evolve
    :: SkullsState vertex
    -> Either GameError Event
    -> EvolutionResult SkullsTopology SkullsState vertex (Either GameError Event)
  evolve state eitherErrorEvent
    = case eitherErrorEvent of
        -- Error? Nothing changes.
        Left _ -> EvolutionResult state
        -- Otherwise, we can do something
        Right event -> case (state, event) of
          (SkullsInitialState, GameStarted initialData) ->
            initialResult initialData

          -- Place a card; i.e. update the cards the player has and move to
          -- the next player.
          (SkullsPlacingCardsState stateData, CardPlayed turnData) ->
            let newStacks = insertWith (++) (turnData.playerId) [turnData.card] (stateData.playerStacks)
             in EvolutionResult $ SkullsPlacingCardsState $
                 StateData { currentPlayer = nextPlayer (stateData.totalPlayers) (turnData.playerId)
                           , playerStacks = newStacks
                           , winCounts = stateData.winCounts
                           , totalPlayers = stateData.totalPlayers
                           }

          -- Make a bet.


  initialResult
    :: (AllowedTransition SkullsTopology initialVertex PlacingCards)
    => InitialData
    -> EvolutionResult SkullsTopology SkullsState initialVertex (Either GameError Event)
  initialResult initialData =
    EvolutionResult $
      SkullsPlacingCardsState $
        StateData
          { currentPlayer = initialData.startingPlayer
          , playerStacks  = mempty
          , totalPlayers  = initialData.players
          , winCounts     = mempty
          }
