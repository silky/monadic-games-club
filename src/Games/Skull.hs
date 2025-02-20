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

import "containers" Data.Map (Map, insertWith)
import "containers" Data.Map qualified as Map
import "crem" Crem.BaseMachine (InitialState (..))
import "crem" Crem.Decider (Decider (..), EvolutionResult (..))
import "crem" Crem.Render.RenderableVertices (AllVertices (..), RenderableVertices)
import "crem" Crem.Topology
import "singletons-base" Data.Singletons.Base.TH
import "base" Data.Either (isRight)

-- TODO:
--
--  - [ ] Limit the number of cards.

-- * Domain

data Suit
  = Red
  | Green
  | Blue
  | Orange
  | Pink
  | Black
  deriving stock (Eq, Show)

data Card
  = Flower Suit
  | Skull  Suit
  deriving stock (Eq, Show)

isSkull :: Card -> Bool
isSkull (Skull _)  = True
isSkull (Flower _) = False

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
  -- * Betting
  | MakeBet BetData
  | PassBet PassData
  -- * Resolving a bet
  | PickUpMyStack
  | PickUpFrom PlayerId
  | PickedUpSuccessfully Int
  deriving stock (Show)

data Event
  = GameStarted InitialData
  | CardPlayed  TurnData
  | BetMade     BetData
  | BetPassed
  | PickedUp    Int
  | WonRound
  -- | BetResolved  Outcome
  | FailedBet -- It's okay to fail a bet actually.


-- data Outcome

-- * Topology

$( singletons
    [d|
      data SkullsVertex
        = Initial
        | PlacingCards
        | Betting
        | GameOver
        | ResolvingBet
        deriving stock (Eq, Show, Enum, Bounded)

      skullsTopology :: Topology SkullsVertex
      skullsTopology = Topology [ (Initial,      [PlacingCards])
                                , (PlacingCards, [Betting])
                                , (Betting,      [ResolvingBet])
                                , (ResolvingBet, [GameOver, PlacingCards])
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

countWins :: PlayerId -> StateData -> Int
countWins playerId stateData =
  case Map.lookup playerId stateData.winCounts of
    Nothing -> 0
    Just n -> n

recordWin :: PlayerId -> StateData -> StateData
recordWin playerId stateData =
  stateData { winCounts = Map.insertWith (+) playerId 1 stateData.winCounts }

advancePlayer :: StateData -> StateData
advancePlayer stateData@StateData{currentPlayer, totalPlayers} =
  stateData { currentPlayer = nextPlayer totalPlayers currentPlayer }

totalCardsPlayed :: StateData -> Int
totalCardsPlayed StateData{playerStacks} =
  Map.foldl (\len a -> len + length a) 0 playerStacks

-- | Take n cards from the entry for the specific player. If we found a skull,
-- just fail with `HitSkull`.
takeNCardsFrom :: Int -> PlayerId -> StateData -> Either HitSkull StateData
takeNCardsFrom n playerId stateData =
  if anySkulls
     then Left HitSkull
     else Right newStateData
  where
    Just cards   = Map.lookup playerId stateData.playerStacks
    anySkulls    = any isSkull (take n cards)
    newStateData = stateData { playerStacks = Map.insert playerId (drop n cards) stateData.playerStacks }

countStackOfPlayer :: PlayerId -> StateData -> Int
countStackOfPlayer playerId stateData =
  case Map.lookup playerId stateData.playerStacks of
    Nothing -> 0
    Just xs -> length xs


data HitSkull = HitSkull

data BetStateData = BetStateData
  { highestBet  :: BetData
  -- | A player has made a bet if this is Just.
  , playersBets :: Map PlayerId (Maybe BetData)
  }

bettingFinished :: BetStateData -> StateData -> Bool
bettingFinished betStateData stateData =
  bets == stateData.totalPlayers
    where
      counts = Map.map (maybe 0 (const 1)) betStateData.playersBets
      bets   = Map.foldl (+) 0 counts

data ResolvingBetStateData = ResolvingBetStateData
  { flowersToPickUp :: Int
  , currentFlowersPickedUp :: Int
  , playerId :: PlayerId
  }

pickedUpEnough :: ResolvingBetStateData -> Bool
pickedUpEnough state = state.flowersToPickUp == state.currentFlowersPickedUp

data SkullsState (vertex :: SkullsVertex) where
  SkullsInitialState      :: SkullsState Initial
  SkullsPlacingCardsState :: StateData -> SkullsState PlacingCards
  SkullsBettingSate       :: BetStateData -> StateData -> SkullsState Betting
  SkullsResolvingBet      :: ResolvingBetStateData -> StateData -> SkullsState ResolvingBet
  SkullsGameOverState     :: PlayerId -> SkullsState GameOver

data GameError
  -- ** Starting
  = TooFewPlayers
  | GameAlreadyStarted

  -- ** Placing cards
  | GameNotStarted
  | CantPlaceWhileBetting

  -- ** Betting
  | BetMustBeHigher
  | CantPassNow
  | BetTooLarge

  -- ** Resolving a bet
  | PickedUpSkull

  -- ** Generic
  | NotYourTurn
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

  (SkullsPlacingCardsState _, PassBet _) -> Left CantPassNow

  -- ** Making a bet
  (SkullsPlacingCardsState stateData, MakeBet bet)
    | stateData.currentPlayer /= bet.playerId -> Left NotYourTurn
    | bet.flowers <= totalCardsPlayed stateData -> Right $ BetMade bet
    | otherwise -> Left $ BetTooLarge

  (SkullsBettingSate (BetStateData{highestBet}) stateData, MakeBet newBet)
    | stateData.currentPlayer /= newBet.playerId -> Left NotYourTurn
    | newBet.flowers <= highestBet.flowers -> Left BetMustBeHigher
    | otherwise -> Right $ BetMade newBet

  (SkullsBettingSate _ stateData, PassBet newBet)
    | stateData.currentPlayer /= newBet.playerId -> Left NotYourTurn
    | otherwise -> Right $ BetPassed

  -- ** Resolving a bet. Who won?!
  -- TODO: Implement
  -- (SkullsResolvingBet betToWin stateData, ResolveBet)
  --   | otherwise -> undefined

  (SkullsResolvingBet betToWin stateData, PickUpMyStack)
    | stateData.currentPlayer /= betToWin.playerId -> Left NotYourTurn
    | otherwise ->
        let myCards = countStackOfPlayer stateData.currentPlayer stateData
            -- The maximum we can take from my deck is the number of cards I have.
            toTake  = min betToWin.flowersToPickUp myCards
        in case takeNCardsFrom toTake stateData.currentPlayer stateData of
              Right _ ->
                if toTake <= myCards
                  -- If we don't need to take more; we're done! We win the round.
                  then Right $ WonRound
                  -- If we do, keep picking up.
                  else Right $ PickedUp toTake
              Left _  -> Right FailedBet

  (SkullsResolvingBet betToWin stateData, PickUpFrom playerId)
    | otherwise -> undefined

  -- ** Bookkeeping
  -- (SkullsFinishedState {}, _) -> Left GameIsOver

-- | Perfoms state transitions.
evolveSkulls
  :: SkullsState vertex
  -> Either GameError Event
  -> EvolutionResult SkullsTopology SkullsState vertex (Either GameError Event)
evolveSkulls state eitherErrorEvent = case eitherErrorEvent of
  -- Error? Nothing changes.
  Left _ -> EvolutionResult state

  -- Otherwise, we can do something
  Right event -> case (state, event) of
    (SkullsInitialState, GameStarted initialData) ->
      initialResult initialData

    -- ** Place a card.
    -- i.e. update the cards the player has and move to the next player.SkullsInitialState
    (SkullsPlacingCardsState stateData, CardPlayed turnData) ->
      let newStacks = insertWith (++) (turnData.playerId) [turnData.card] (stateData.playerStacks)
        in EvolutionResult $ SkullsPlacingCardsState $
          (advancePlayer stateData) { playerStacks = newStacks }

    -- Record the first bet, go into the betting state.
    (SkullsPlacingCardsState stateData, BetMade betData) ->
      EvolutionResult $ SkullsBettingSate
        (BetStateData
          { highestBet = betData
          , playersBets = Map.singleton stateData.currentPlayer (Just betData)
          }
        )
        (advancePlayer stateData)

    -- ** Someone raised a bet.
    -- Throw away the last highest bet, and optionally go into bet resolution,
    -- if everyone has passed.
    (SkullsBettingSate betStateData stateData, BetMade newBet) ->
      if bettingFinished betStateData stateData
          then EvolutionResult $ SkullsResolvingBet
            (ResolvingBetStateData
              { flowersToPickUp = newBet.flowers
              , currentFlowersPickedUp = 0
              -- TODO: Is this right? I think so.
              , playerId = (advancePlayer stateData).currentPlayer
              })
            (advancePlayer stateData)
          else EvolutionResult $ SkullsBettingSate betStateData (advancePlayer stateData)

    (SkullsResolvingBet betStateData stateData, WonRound) ->
      let wins = countWins betStateData.playerId stateData
       in if wins < 2
             then EvolutionResult $ SkullsPlacingCardsState (recordWin betStateData.playerId stateData)
             -- They won!
             else EvolutionResult $ SkullsGameOverState betStateData.playerId

initialResult
  :: (AllowedTransition SkullsTopology initialVertex PlacingCards)
  => InitialData
  -> EvolutionResult SkullsTopology SkullsState initialVertex (Either GameError Event)
initialResult initialData =
  EvolutionResult $
    SkullsPlacingCardsState $
      StateData
        { currentPlayer = initialData.startingPlayer
        , totalPlayers  = initialData.players
        , playerStacks  = mempty
        , winCounts     = mempty
        }
