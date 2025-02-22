{-# language DuplicateRecordFields #-}
{-# language OverloadedRecordDot   #-}
{-# language TemplateHaskell       #-}
{-# language UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-unused-type-patterns #-}

module Game.Skulls.Model where

{-
  Skulls is a simple betting-style game.

  The simple version is: You have 4 flowers and one skull. You can place
  either of these, face down, and play follows this way until someone decides
  to make a "Bet" for how many cards they can pick up without hitting a Skull
  (starting with their own pile!)

  They win the round if they do that; or lose if they don't.

  If someone wins twice, they win the game.
-}

import "base" Data.Function ((&))
import "base" Data.Functor.Identity (Identity)
import "containers" Data.Map (Map, insertWith)
import "containers" Data.Map qualified as Map
import "crem" Crem.BaseMachine (InitialState (..))
import "crem" Crem.Decider (Decider (..), deciderMachine, EvolutionResult (..))
import "crem" Crem.Render.Render
import "crem" Crem.Render.RenderableVertices (AllVertices (..), RenderableVertices)
import "crem" Crem.StateMachine (StateMachineT (Basic))
import "crem" Crem.Topology
import "singletons-base" Data.Singletons.Base.TH
import "text" Data.Text (unpack)
import "extra" Data.Tuple.Extra ((&&&))
import "aeson" Data.Aeson
import "base" GHC.Generics (Generic)
import "elm-street" Elm (Elm, ElmStreet (..), ElmDefinition(DefPrim), ElmPrim(ElmList))
import "elm-street" Elm.Generic (toElmDefinition, elmRef)

-- TODO:
--
--  - [ ] Test game play
--  - [ ] Generate gameplay quickcheck and count how many times we et into
--        each state.
--  - [ ] Double-check that the business about advancing players works
--  - [ ] Do other checks of current player
--  - [ ] Maybe tidy-up the current player stuff in general

-- * Domain


data Suit
  = Red
  | Green
  | Blue
  | Orange
  | Pink
  | Black
  deriving stock (Eq, Show, Generic)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet Suit

data Card
  = Flower Suit
  | Skull  Suit
  deriving stock (Eq, Show, Generic)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet Card

maxFlowers :: Int
maxFlowers = 4

maxCards :: Int
maxCards = maxFlowers + 1

isSkull :: Card -> Bool
isSkull (Skull _)  = True
isSkull (Flower _) = False

newtype PlayerId = PlayerId Int
  deriving stock (Generic)
  deriving newtype (Eq, Show, Num, Ord, ToJSON, FromJSON)
  deriving anyclass (ToJSONKey, FromJSONKey)
  deriving (Elm) via ElmStreet PlayerId

newtype PlayerCount = PlayerCount Int
  deriving stock (Generic)
  deriving newtype (Eq, Show, Num, Ord, ToJSON, FromJSON)
  deriving (Elm) via ElmStreet PlayerCount

-- | Players start counting at zero because we use mod to keep track of player
-- ids
firstPlayer :: PlayerId
firstPlayer = PlayerId 0

nextPlayer :: PlayerCount -> PlayerId -> PlayerId
nextPlayer (PlayerCount count) (PlayerId i)
  = PlayerId $ (i + 1 `mod` count)

-- * Commands and events

-- | Data to kick off a game.
data InitialData = InitialData
  { idPlayers :: PlayerCount
  , idStartingPlayer :: PlayerId
  }
  deriving stock (Eq, Show, Generic)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet InitialData

-- | This is what happens in a turn.
data TurnData = TurnData
  { tdPlayerId :: PlayerId
  , tdCard :: Card
  }
  deriving stock (Eq, Show, Generic)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet TurnData

data BetData = BetData
  { bdFlowers :: Int
  , bdPlayerId :: PlayerId
  }
  deriving stock (Eq, Show, Generic)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet BetData

data PassData = PassData
  { pdPlayerId :: PlayerId
  }
  deriving stock (Eq, Show, Generic)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet PassData

data Command
  = StartGame InitialData
  | PlayCard TurnData
  -- * Betting
  | MakeBet BetData
  | PassBet PassData
  -- * Resolving a bet
  | PickUpMyStack
  | PickUpFrom PlayerId
  deriving stock (Eq, Show, Generic)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet Command

data Event
  = GameStarted InitialData
  | CardPlayed  TurnData
  | BetMade     BetData
  | BetPassed
  | PickedUp    Int
  | WonRound
  | WonGame
  | FailedBet -- It's okay to fail a bet actually.
  deriving stock (Eq, Show, Generic)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet Event

-- * Topology

$( singletons
    [d|
      data SkullsVertex
        = Initial
        | PlacingCards
        | Betting
        | ResolvingBet
        | GameOver
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

-- Hacks for elm-street not supporting Maps (because Elm doesn't support
-- them).
instance Elm (Map PlayerId [Card]) where
  toElmDefinition _ = DefPrim $ ElmList (elmRef @(PlayerId, [Card]))

instance {-# OVERLAPPING #-} ToJSON   (Map PlayerId [Card])
  where toJSON m = toJSON $ Map.toList m

instance {-# OVERLAPPING #-} FromJSON (Map PlayerId [Card])
  where parseJSON v = Map.fromList <$> parseJSON @[(PlayerId, [Card])] v

instance Elm (Map PlayerId Int) where
  toElmDefinition _ = DefPrim $ ElmList (elmRef @(PlayerId, Int))

instance {-# OVERLAPPING #-} ToJSON   (Map PlayerId Int)
  where toJSON m = toJSON $ Map.toList m

instance {-# OVERLAPPING #-} FromJSON (Map PlayerId Int)
  where parseJSON v = Map.fromList <$> parseJSON @[(PlayerId, Int)] v

instance Elm (Map PlayerId (Maybe BetData)) where
  toElmDefinition _ = DefPrim $ ElmList (elmRef @(PlayerId, Maybe BetData))

instance {-# OVERLAPPING #-} ToJSON   (Map PlayerId (Maybe BetData))
  where toJSON m = toJSON $ Map.toList m

instance {-# OVERLAPPING #-} FromJSON (Map PlayerId (Maybe BetData))
  where parseJSON v = Map.fromList <$> parseJSON @[(PlayerId, Maybe BetData)] v

data StateData = StateData
  { sdCurrentPlayer :: PlayerId
  , sdPlayerStacks  :: Map PlayerId [Card]
  , sdWinCounts     :: Map PlayerId Int
  , sdLossCounts    :: Map PlayerId Int
  , sdTotalPlayers  :: PlayerCount
  }
  deriving stock (Eq, Show, Generic)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet StateData

countWins :: PlayerId -> StateData -> Int
countWins playerId stateData =
  maybe 0 id $ Map.lookup playerId stateData.sdWinCounts

playedSkull :: PlayerId -> StateData -> Bool
playedSkull playerId stateData =
  let cards = maybe [] id (Map.lookup playerId stateData.sdPlayerStacks)
   in any isSkull cards

recordWin :: PlayerId -> StateData -> StateData
recordWin playerId stateData =
  stateData { sdWinCounts = Map.insertWith (+) playerId 1 stateData.sdWinCounts }

recordLoss :: PlayerId -> StateData -> StateData
recordLoss playerId stateData =
  stateData { sdLossCounts = Map.insertWith (+) playerId 1 stateData.sdLossCounts }

advancePlayer :: StateData -> StateData
advancePlayer stateData@StateData{sdCurrentPlayer, sdTotalPlayers} =
  stateData { sdCurrentPlayer = nextPlayer sdTotalPlayers sdCurrentPlayer }

totalCardsPlayed :: StateData -> Int
totalCardsPlayed StateData{sdPlayerStacks} =
  Map.foldl (\len a -> len + length a) 0 sdPlayerStacks

-- | Take n cards from the entry for the specific player. If we found a skull,
-- just fail with `HitSkull`.
takeNCardsFrom :: Int -> PlayerId -> StateData -> Either HitSkull StateData
takeNCardsFrom n playerId stateData =
  if anySkulls
     then Left HitSkull
     else Right newStateData
  where
    -- We can just pattern match here; it can't given an error.
    cards        = maybe [] id (Map.lookup playerId stateData.sdPlayerStacks)
    anySkulls    = any isSkull (take n cards)
    newStateData = stateData { sdPlayerStacks = Map.insert playerId (drop n cards) stateData.sdPlayerStacks }

countStackOfPlayer :: PlayerId -> StateData -> Int
countStackOfPlayer playerId stateData =
  maybe 0 length $ Map.lookup playerId stateData.sdPlayerStacks

data HitSkull = HitSkull
  deriving stock (Eq, Show, Generic)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet HitSkull

data BetStateData = BetStateData
  { bsdHighestBet  :: BetData
  , bsdPlayersBets :: Map PlayerId (Maybe BetData)
  }
  deriving stock (Eq, Show, Generic)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet BetStateData

bettingFinished :: BetStateData -> StateData -> Bool
bettingFinished betStateData stateData =
  bets == stateData.sdTotalPlayers
    where
      counts = Map.map (maybe 0 (const 1)) betStateData.bsdPlayersBets
      bets   = Map.foldl (+) 0 counts

data ResolvingBetStateData = ResolvingBetStateData
  { rbsdFlowersToPickUp :: Int
  , rbsdCurrentFlowersPickedUp :: Int
  , rbsdPlayerId :: PlayerId
  }
  deriving stock (Eq, Show, Generic)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet ResolvingBetStateData

pickedUpEnough :: ResolvingBetStateData -> Bool
pickedUpEnough state = state.rbsdFlowersToPickUp == state.rbsdCurrentFlowersPickedUp

data SkullsState (vertex :: SkullsVertex) where
  SkullsInitialState      :: SkullsState Initial
  SkullsPlacingCardsState :: StateData -> SkullsState PlacingCards
  SkullsBettingState      :: BetStateData -> StateData -> SkullsState Betting
  SkullsResolvingBetState :: ResolvingBetStateData -> StateData -> SkullsState ResolvingBet
  SkullsGameOverState     :: PlayerId -> SkullsState GameOver

data GameError
  -- ** Starting
  = TooFewPlayers
  | GameAlreadyStarted

  -- ** Placing cards
  | GameNotStarted
  | CantPlaceWhileBetting
  | CantPickUpNow
  | NoMoreCardsToPlay
  | PlayedYourSkull

  -- ** Betting
  | BetMustBeHigher
  | CantPassNow
  | BetTooLarge
  | CanOnlyResolveBetNow

  -- ** Resolving a bet
  | PickedUpSkull

  -- ** Generic
  | NotYourTurn
  | GameIsOver
  deriving stock (Eq, Show, Generic)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet GameError


-- * Machine

decider
  :: InitialState SkullsState
  -> Decider SkullsTopology Command GameResult
decider initialState =
  Decider
    { deciderInitialState = initialState
    , decide = decideSkulls
    , evolve = evolveSkulls
    }

data StateBlob
  = BlobInitialState
  | BlobPlacingCardsState StateData
  | BlobBettingState BetStateData StateData
  | BlobResolvingBetState ResolvingBetStateData StateData
  | BlobGameOverState PlayerId
  deriving stock (Eq, Show, Generic)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet StateBlob

newtype GameResult = GameResult (Either GameError (Event, StateBlob))
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)
  deriving (Elm) via ElmStreet GameResult

makeStateBlob :: SkullsState vertex -> StateBlob
makeStateBlob = \case
  SkullsInitialState -> BlobInitialState
  SkullsPlacingCardsState s -> BlobPlacingCardsState s
  SkullsBettingState b s -> BlobBettingState b s
  SkullsResolvingBetState r s -> BlobResolvingBetState r s
  SkullsGameOverState p -> BlobGameOverState p

-- TODO: This is wrong! The state it's referring to here is the _past_ state.
--
-- In fact I think this means we might need to switch entirely away from the
-- 'decider' model; because in that setting we can't get the actual state
-- output until the evolve call, which we do not have access to.
decideSkulls :: Command -> SkullsState vertex -> GameResult
decideSkulls command = GameResult . g . (decideSkulls' command &&& makeStateBlob)
  where
    g :: (Either a b, c) -> Either a (b, c)
    g = \case
      (Left l, _) -> Left l
      (Right b, c) -> Right (b, c)

-- | Decides the details of which events can trigger state transitions.
decideSkulls' :: Command -> SkullsState vertex -> Either GameError Event
decideSkulls' command state = case (state, command) of
  -- ** Starting a game
  (SkullsInitialState, StartGame initialData)
    | initialData.idPlayers < PlayerCount 2 -> Left TooFewPlayers
    | otherwise -> Right $ GameStarted initialData

  (SkullsInitialState,         _)            -> Left GameNotStarted
  (SkullsPlacingCardsState {}, StartGame _)  -> Left GameAlreadyStarted
  (SkullsBettingState {},       StartGame _) -> Left GameAlreadyStarted

  -- ** Playing a card
  (SkullsPlacingCardsState stateData, PlayCard turnData)
    | stateData.sdCurrentPlayer /= turnData.tdPlayerId -> Left NotYourTurn
    | countStackOfPlayer stateData.sdCurrentPlayer stateData == maxCards
        -> Left NoMoreCardsToPlay
    | playedSkull stateData.sdCurrentPlayer stateData
        -> Left PlayedYourSkull
    | otherwise -> Right $ CardPlayed turnData

  (SkullsBettingState {}, PlayCard _) -> Left CantPlaceWhileBetting

  (SkullsPlacingCardsState _, PassBet _) -> Left CantPassNow

  -- ** Making a bet
  (SkullsPlacingCardsState stateData, MakeBet bet)
    | stateData.sdCurrentPlayer /= bet.bdPlayerId -> Left NotYourTurn
    | bet.bdFlowers <= totalCardsPlayed stateData -> Right $ BetMade bet
    | otherwise -> Left $ BetTooLarge

  (SkullsBettingState (BetStateData{bsdHighestBet}) stateData, MakeBet newBet)
    | stateData.sdCurrentPlayer /= newBet.bdPlayerId -> Left NotYourTurn
    | newBet.bdFlowers <= bsdHighestBet.bdFlowers -> Left BetMustBeHigher
    | otherwise -> Right $ BetMade newBet

  (SkullsBettingState _ stateData, PassBet newBet)
    | stateData.sdCurrentPlayer /= newBet.pdPlayerId -> Left NotYourTurn
    | otherwise -> Right $ BetPassed

  (SkullsResolvingBetState betToWin stateData, PickUpMyStack)
    | stateData.sdCurrentPlayer /= betToWin.rbsdPlayerId -> Left NotYourTurn
    | otherwise -> attemptPickupFrom stateData.sdCurrentPlayer betToWin.rbsdFlowersToPickUp stateData

  (SkullsResolvingBetState betToWin stateData, PickUpFrom playerId)
    | stateData.sdCurrentPlayer /= betToWin.rbsdPlayerId -> Left NotYourTurn
    | otherwise -> attemptPickupFrom playerId betToWin.rbsdFlowersToPickUp stateData

  -- ** Bookkeeping
  -- Can't do anything if the same is over.
  (SkullsGameOverState {}, _) -> Left GameIsOver

  -- Can only pick up while betting
  (_, PickUpMyStack) -> Left CantPickUpNow
  (_, PickUpFrom {}) -> Left CantPickUpNow

  -- If you're resolving bet, you can only be picking up.
  (SkullsResolvingBetState {}, _) -> Left CanOnlyResolveBetNow

attemptPickupFrom :: PlayerId -> Int -> StateData -> Either GameError Event
attemptPickupFrom playerId flowers stateData =
  let cards  = countStackOfPlayer playerId stateData
      toTake = min flowers cards
  in case takeNCardsFrom toTake playerId stateData of
        -- ** We successfully took N cards.
        --
        -- Maybe we won, or maybe we keep going.
        Right _ ->
          if toTake <= cards
            -- If we don't need to take more; we're done! We win the round.
            -- Let's check if we also win the game.
            then
              let wins = countWins playerId stateData
               in if wins == 2
                     then Right WonGame
                     else Right WonRound
            -- If we do, keep picking up.
            else Right $ PickedUp toTake
        --
        -- ** We failed; we hit a skull.
        Left _  -> Right FailedBet

-- | Perfoms state transitions.
evolveSkulls
  :: SkullsState vertex
  -> GameResult
  -> EvolutionResult SkullsTopology SkullsState vertex GameResult
evolveSkulls state (GameResult eitherErrorEvent) =
  case eitherErrorEvent of
  -- Error? Nothing changes.
  Left _ -> EvolutionResult state

  -- Otherwise, we can do something
  Right (event, _) -> case (state, event) of
    (SkullsInitialState, GameStarted initialData) ->
      initialResult initialData

    (SkullsInitialState, _)    -> EvolutionResult state
    (SkullsGameOverState {}, _) -> EvolutionResult state

    -- ** Place a card.
    -- i.e. update the cards the player has and move to the next player.SkullsInitialState
    (SkullsPlacingCardsState stateData, CardPlayed turnData) ->
      let newStacks = insertWith (++) (turnData.tdPlayerId) [turnData.tdCard] (stateData.sdPlayerStacks)
        in EvolutionResult $ SkullsPlacingCardsState $
          (advancePlayer stateData) { sdPlayerStacks = newStacks }

    -- Record the first bet, go into the betting state.
    (SkullsPlacingCardsState stateData, BetMade betData) ->
      EvolutionResult $ SkullsBettingState
        (BetStateData
          { bsdHighestBet = betData
          , bsdPlayersBets = Map.singleton stateData.sdCurrentPlayer (Just betData)
          }
        )
        (advancePlayer stateData)

    (SkullsPlacingCardsState {}, _) -> EvolutionResult state

    -- ** Someone raised a bet.
    -- Throw away the last highest bet, and optionally go into bet resolution,
    -- if everyone has passed.
    (SkullsBettingState betStateData stateData, BetMade newBet) ->
      -- In betting, we still move around Clockwise
      let newState = advancePlayer stateData
      in if bettingFinished betStateData stateData
            then EvolutionResult $ SkullsResolvingBetState
                  (ResolvingBetStateData
                    { rbsdFlowersToPickUp = newBet.bdFlowers
                    , rbsdCurrentFlowersPickedUp = 0
                    , rbsdPlayerId = newBet.bdPlayerId -- The player who made the highest bet
                    })
                  newState
           else EvolutionResult $ SkullsBettingState betStateData newState


    (SkullsBettingState {}, _) -> EvolutionResult state

    -- ** Won a bet.
    (SkullsResolvingBetState betStateData stateData, WonRound) ->
      let newState =
            stateData
              { sdPlayerStacks = mempty
              -- TODO: Mabe the next player changes here; it's not just the first.
              , sdCurrentPlayer = firstPlayer
              } & recordWin betStateData.rbsdPlayerId
       in EvolutionResult $ SkullsPlacingCardsState newState

    -- If you've won two; you win! Otherwise, keep playing.
    (SkullsResolvingBetState betStateData _, WonGame) ->
      EvolutionResult $ SkullsGameOverState betStateData.rbsdPlayerId

    -- ** Failed a bet; just count it and go back to the start.
    (SkullsResolvingBetState betStateData stateData, FailedBet) ->
      let newState =
            stateData
              { sdPlayerStacks = mempty
                -- TODO: Mabe the next player changes here; it's not just the first.
              , sdCurrentPlayer = firstPlayer
              } & recordLoss betStateData.rbsdPlayerId
      in EvolutionResult $ SkullsPlacingCardsState newState

    (SkullsResolvingBetState {}, _) -> EvolutionResult state


initialResult
  :: (AllowedTransition SkullsTopology initialVertex PlacingCards)
  => InitialData
  -> EvolutionResult SkullsTopology SkullsState initialVertex GameResult
initialResult initialData =
  EvolutionResult $
    SkullsPlacingCardsState $
      StateData
        { sdCurrentPlayer = initialData.idStartingPlayer
        , sdTotalPlayers  = initialData.idPlayers
        , sdPlayerStacks  = mempty
        , sdWinCounts     = mempty
        , sdLossCounts    = mempty
        }


-- * Rendering

printMermaid :: IO ()
printMermaid = putStrLn $ unpack t
    where
      Mermaid t = renderUntypedGraph (machineAsGraph m)
      m :: StateMachineT Identity Command GameResult
      m = Basic $ deciderMachine (decider $ InitialState SkullsInitialState)
