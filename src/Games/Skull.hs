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
import "containers" Data.Map qualified as Map
import "crem" Crem.BaseMachine (InitialState (..))
import "crem" Crem.Decider (Decider (..), EvolutionResult (..))
import "crem" Crem.Render.RenderableVertices (AllVertices (..), RenderableVertices)
import "crem" Crem.Topology
import "singletons-base" Data.Singletons.Base.TH

-- TODO: Tidyup the semantics around betting.
--
--  - [ ] Make sure we're transitioning the player at each step (either a bet
--        or placing a card.)
--  - [ ] Maybe unify the check for `currentPlayer` so it's at the top and
--        once, instead of all the same.

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
  = PlayerId $ (i + 1 `mod` count) + 1

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

data BetData = BetData
  { flowers :: Int
  , playerId :: PlayerId
  }
  deriving stock (Eq, Show)

data PassData = PassData
  { playerId :: PlayerId
  }
  deriving stock (Eq, Show)

data Command
  = StartGame InitialData
  | PlayCard TurnData
  | MakeBet BetData
  | PassBet PassData
  deriving stock (Show)

data Event
  = GameStarted InitialData
  | CardPlayed  TurnData
  | BetMade     BetData
  | BetPassed


-- * Topology

$( singletons
    [d|
      data SkullsVertex
        = Initial
        | PlacingCards
        | Betting
        | Finished
        | ResolveBet
        deriving stock (Eq, Show, Enum, Bounded)

      skullsTopology :: Topology SkullsVertex
      skullsTopology = Topology [ (Initial,      [PlacingCards])
                                , (PlacingCards, [Betting])
                                , (Betting,      [ResolveBet])
                                , (ResolveBet,   [PlacingCards, Finished])
                                ]
      |]
 )

deriving via AllVertices SkullsVertex instance RenderableVertices SkullsVertex


-- * State

data StateData = StateData
  { currentPlayer     :: PlayerId
  , playerStacks      :: Map PlayerId [Card]
  , winCounts         :: Map PlayerId Int
  , totalPlayers      :: PlayerCount
  }

totalCardsPlayed :: StateData -> Int
totalCardsPlayed StateData{playerStacks} =
  Map.foldl (\len a -> len + length a) 0 playerStacks

-- TODO: Implement
lastPlayerToBet :: StateData -> Bool
lastPlayerToBet StateData{currentPlayer = PlayerId p, totalPlayers = PlayerCount n} =
  undefined

data SkullsState (vertex :: SkullsVertex) where
  SkullsInitialState      :: SkullsState Initial
  SkullsPlacingCardsState :: StateData -> SkullsState PlacingCards
  SkullsBettingSate       :: BetData -> StateData -> SkullsState Betting
  SkullsResolveBet        :: BetData -> StateData -> SkullsState ResolveBet
  SkullsFinishedState     :: StateData -> SkullsState Finished

data GameError
  -- Starting
  = TooFewPlayers
  | GameAlreadyStarted
  --
  -- Placing cards
  | GameNotStarted
  | NotYourTurn
  | CantPlaceWhileBetting
  --
  -- Betting
  | BetMustBeHigher
  | CantPassNow
  | BetTooLarge
  --
  -- Generic
  | GameIsOver
  deriving stock (Eq, Show)

-- * Machine

decider
  :: InitialState SkullsState
  -> Decider SkullsTopology Command (Either GameError Event)
decider initialState =
  Decider
    { deciderInitialState = initialState
    , decide = decideSkulls
    , evolve = evolveSkulls
    }


-- | Decides the details of which events can trigger state transitions.
decideSkulls :: Command -> SkullsState vertex -> Either GameError Event
decideSkulls command state = case (state, command) of
  -- ** Starting a game
  (SkullsInitialState, StartGame initialData)
    | initialData.players < PlayerCount 2 -> Left TooFewPlayers
    | otherwise -> Right $ GameStarted initialData

  (SkullsInitialState,         _)           -> Left GameNotStarted
  (SkullsPlacingCardsState {}, StartGame _) -> Left GameAlreadyStarted
  (SkullsBettingSate {},       StartGame _) -> Left GameAlreadyStarted

  -- ** Playing a card
  (SkullsPlacingCardsState stateData, PlayCard turnData)
    | stateData.currentPlayer /= turnData.playerId -> Left NotYourTurn
    | otherwise -> Right $ CardPlayed turnData

  (SkullsBettingSate {}, PlayCard _) -> Left CantPlaceWhileBetting

  (SkullsPlacingCardsState _, PassBet _) ->
    Left CantPassNow

  -- ** Making a bet
  (SkullsPlacingCardsState stateData, MakeBet bet)
    | stateData.currentPlayer /= bet.playerId -> Left NotYourTurn
    | bet.flowers <= totalCardsPlayed stateData -> Right $ BetMade bet
    | otherwise -> Left $ BetTooLarge

  (SkullsBettingSate highestBet stateData, MakeBet newBet)
    | stateData.currentPlayer /= newBet.playerId -> Left NotYourTurn
    | newBet.flowers <= highestBet.flowers -> Left BetMustBeHigher
    | otherwise -> Right $ BetMade newBet

  (SkullsBettingSate _ stateData, PassBet newBet)
    | stateData.currentPlayer /= newBet.playerId -> Left NotYourTurn
    | otherwise -> Right $ BetPassed

  -- ** Bookkeeping
  (SkullsFinishedState {}, _) -> Left GameIsOver


-- | Perfoms state transitions.
evolveSkulls
  :: SkullsState vertex
  -> Either GameError Event
  -> EvolutionResult SkullsTopology SkullsState vertex (Either GameError Event)
evolveSkulls state eitherErrorEvent
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
              (advancePlayer stateData) { playerStacks = newStacks }

        -- Record the first bet, go into the betting state.
        (SkullsPlacingCardsState stateData, BetMade betData) ->
          EvolutionResult $ SkullsBettingSate betData (advancePlayer stateData)

        -- Someone raised a bet; throw away the last highest bet, and
        -- optionally go into bet resolution, if we last player played.
        (SkullsBettingSate _ stateData, BetMade newBet) ->
          if lastPlayerToBet stateData
              then EvolutionResult $ SkullsResolveBet  newBet (advancePlayer stateData)
              else EvolutionResult $ SkullsBettingSate newBet (advancePlayer stateData)

advancePlayer :: StateData -> StateData
advancePlayer stateData@StateData{currentPlayer, totalPlayers} =
  stateData { currentPlayer = nextPlayer totalPlayers currentPlayer }

initialResult
  :: (AllowedTransition SkullsTopology initialVertex PlacingCards)
  => InitialData
  -> EvolutionResult SkullsTopology SkullsState initialVertex (Either GameError Event)
initialResult initialData =
  EvolutionResult $
    SkullsPlacingCardsState $
      StateData
        { currentPlayer     = initialData.startingPlayer
        , totalPlayers      = initialData.players
        , playerStacks      = mempty
        , winCounts         = mempty
        }
