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

import "aeson" Data.Aeson
import "base" Data.Function ((&))
import "base" Data.Functor.Identity (Identity)
import "base" GHC.Generics (Generic)
import "containers" Data.Map (Map, insertWith)
import "containers" Data.Map qualified as Map
import "crem" Crem.BaseMachine (pureResult, ActionResult, InitialState (..), BaseMachine, BaseMachineT(..) )
import "crem" Crem.Render.Render
import "crem" Crem.Render.RenderableVertices (AllVertices (..), RenderableVertices)
import "crem" Crem.StateMachine (StateMachineT (Basic))
import "crem" Crem.Topology
import "elm-street" Elm (Elm, ElmStreet (..), ElmDefinition(DefPrim), ElmPrim(ElmList))
import "elm-street" Elm.Generic (toElmDefinition, elmRef)
import "singletons-base" Data.Singletons.Base.TH
import "text" Data.Text (unpack)

-- TODO:
--
--  - [ ] Test game play
--  - [ ] Generate gameplay quickcheck and count how many times we et into
--        each state.
--  - [ ] Double-check that the business about advancing players works
--  - [ ] Do other checks of current player
--  - [ ] Maybe tidy-up the current player stuff in general

-- * Domain

data Card
  = Flower
  | Skull
  deriving stock (Eq, Show, Generic)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet Card

maxFlowers :: Int
maxFlowers = 4

maxCards :: Int
maxCards = maxFlowers + 1

isSkull :: Card -> Bool
isSkull Skull = True
isSkull Flower = False

newtype PlayerId = PlayerId Int
  deriving stock (Generic)
  deriving newtype (Eq, Show, Num, Ord, ToJSON, FromJSON)
  deriving anyclass (ToJSONKey, FromJSONKey)
  deriving (Elm) via ElmStreet PlayerId

newtype PlayerCount = PlayerCount { unPlayerCount :: Int }
  deriving stock (Generic)
  deriving newtype (Eq, Show, Num, Ord, ToJSON, FromJSON)
  deriving (Elm) via ElmStreet PlayerCount

-- | Players start counting at zero because we use mod to keep track of player
-- ids
firstPlayer :: PlayerId
firstPlayer = PlayerId 0

nextPlayer :: PlayerCount -> PlayerId -> PlayerId
nextPlayer (PlayerCount count) (PlayerId i)
  = PlayerId $ ((i + 1) `mod` count)

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

instance Elm (Map PlayerId (Either PassData BetData)) where
  toElmDefinition _ = DefPrim $ ElmList (elmRef @(PlayerId, Either PassData BetData))

instance {-# OVERLAPPING #-} ToJSON   (Map PlayerId (Either PassData BetData))
  where toJSON m = toJSON $ Map.toList m

instance {-# OVERLAPPING #-} FromJSON (Map PlayerId (Either PassData BetData))
  where parseJSON v = Map.fromList <$> parseJSON @[(PlayerId, Either PassData BetData)] v

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
  , bsdPlayersBets :: Map PlayerId (Either PassData BetData)
  }
  deriving stock (Eq, Show, Generic)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet BetStateData

-- bettingFinished :: BetStateData -> StateData -> Bool
-- bettingFinished betStateData stateData =
--   bets == unPlayerCount stateData.sdTotalPlayers
--     where
--       bets = length $ Map.toList betStateData.bsdPlayersBets

betWouldFinish :: BetStateData -> StateData -> Bool
betWouldFinish BetStateData{bsdPlayersBets} stateData =
  bets == unPlayerCount stateData.sdTotalPlayers - 1
    where
      bets = length $ Map.toList bsdPlayersBets

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
  | CantBetNow

  -- ** Generic
  | NotYourTurn
  | GameIsOver
  deriving stock (Eq, Show, Generic)
  deriving (Elm, ToJSON, FromJSON) via ElmStreet GameError


-- * Machine

-- decider
--   :: InitialState SkullsState
--   -> Decider SkullsTopology Command GameResult
-- decider initialState =
--   Decider
--     { deciderInitialState = initialState
--     , decide = decideSkulls
--     , evolve = evolveSkulls
--     }

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

gameError
  :: (Applicative m, AllowedTransition SkullsTopology i f)
  => SkullsState f
  -> GameError
  -> ActionResult m SkullsTopology SkullsState i GameResult
gameError s e = pureResult (GameResult . Left $ e) s

gameResult
  :: (Applicative m, AllowedTransition SkullsTopology i f)
  => SkullsState f
  -> Event
  -> ActionResult m SkullsTopology SkullsState i GameResult
gameResult s ev = pureResult (GameResult . Right $ (ev, makeStateBlob s)) s

skullsMachine
  :: BaseMachine SkullsTopology Command GameResult
skullsMachine = BaseMachineT
  { initialState = InitialState SkullsInitialState 
  , action = \state -> case state of
      SkullsInitialState -> \case
        StartGame d
          | d.idPlayers < PlayerCount 2 ->
              gameError state TooFewPlayers

          | otherwise ->
              gameResult (mkInitialState d) $ GameStarted d

        _ -> gameError state GameNotStarted


      SkullsPlacingCardsState stateData -> \case
        PlayCard turnData
          | stateData.sdCurrentPlayer /= turnData.tdPlayerId ->
              gameError state NotYourTurn

          | countStackOfPlayer stateData.sdCurrentPlayer stateData == maxCards ->
              gameError state NoMoreCardsToPlay

          | isSkull turnData.tdCard && playedSkull stateData.sdCurrentPlayer stateData ->
              gameError state PlayedYourSkull

          | otherwise ->
              gameResult (mkCardPlayed stateData turnData) $ CardPlayed turnData

        MakeBet bet
          | stateData.sdCurrentPlayer /= bet.bdPlayerId ->
              gameError state NotYourTurn

          -- Is it a legal bet size? Then that's fine.
          | bet.bdFlowers <= totalCardsPlayed stateData ->
              gameResult (mkFirstBetMade stateData bet) $ BetMade bet

          | otherwise -> gameError state BetTooLarge

        StartGame {}  -> gameError state GameAlreadyStarted
        PassBet {}    -> gameError state CantPassNow
        PickUpMyStack -> gameError state CantPickUpNow
        PickUpFrom {} -> gameError state CantPickUpNow


      SkullsBettingState (betStateData@BetStateData{bsdHighestBet}) stateData -> \case
        StartGame {}   -> gameError state GameAlreadyStarted
        PickUpMyStack  -> gameError state CantPickUpNow
        PickUpFrom {}  -> gameError state CantPickUpNow
        PlayCard {}    -> gameError state CantPlaceWhileBetting
        PassBet newBet
          | stateData.sdCurrentPlayer /= newBet.pdPlayerId ->
              gameError state NotYourTurn

          | otherwise ->
              if betWouldFinish betStateData stateData
                  then gameResult (mkResolvingBet stateData bsdHighestBet) $ BetPassed
                  else gameResult (mkPassBet betStateData stateData newBet) $ BetPassed

        MakeBet newBet
          | stateData.sdCurrentPlayer /= newBet.bdPlayerId ->
              gameError state NotYourTurn

          | newBet.bdFlowers <= bsdHighestBet.bdFlowers ->
              gameError state BetMustBeHigher

          | otherwise ->
              if betWouldFinish betStateData stateData
                  then gameResult (mkResolvingBet stateData newBet) $ BetMade newBet
                  else gameResult (mkRaisingBet betStateData stateData newBet) $ BetMade newBet


      SkullsResolvingBetState betToWin stateData ->
        let pickupFrom p = attemptPickupFrom p betToWin.rbsdFlowersToPickUp stateData
            newState = stateData
              { sdPlayerStacks = initialPlayerStacks stateData.sdTotalPlayers
              -- TODO: Maybe the next player changes here; it's not just the first.
              , sdCurrentPlayer = firstPlayer
              } & recordWin betToWin.rbsdPlayerId
        in \case
        StartGame {} -> gameError state GameAlreadyStarted
        PlayCard {}  -> gameError state CantPlaceWhileBetting
        MakeBet {}   -> gameError state CantBetNow
        PassBet {}   -> gameError state CantPassNow
        PickUpFrom playerId
          | stateData.sdCurrentPlayer /= betToWin.rbsdPlayerId ->
              gameError state NotYourTurn

          | otherwise ->
                case pickupFrom playerId of
                  WonGame   -> gameResult (SkullsGameOverState betToWin.rbsdPlayerId) $ WonGame
                  WonRound  -> gameResult (SkullsPlacingCardsState newState) $ WonRound
                  FailedBet -> gameResult (SkullsPlacingCardsState newState) $ FailedBet
                  -- TODO: This could be resolved by a type-level
                  -- `OneOf of these values.
                  _ -> error "Impossible"

        PickUpMyStack
          | stateData.sdCurrentPlayer /= betToWin.rbsdPlayerId ->
              gameError state NotYourTurn

          | otherwise ->
                case pickupFrom stateData.sdCurrentPlayer of
                  WonGame   -> gameResult (SkullsGameOverState betToWin.rbsdPlayerId) $ WonGame
                  WonRound  -> gameResult (SkullsPlacingCardsState newState) $ WonRound
                  FailedBet -> gameResult (SkullsPlacingCardsState newState) $ FailedBet
                  -- TODO: This could be resolved by a type-level
                  -- `OneOf of these values.
                  _ -> error "Impossible"

      SkullsGameOverState _ -> const $ gameError state GameIsOver
  }
  where
    -- Minor hack: Give them an empty list so that it renders in the UI.
    initialPlayerStacks :: PlayerCount -> Map PlayerId [Card]
    initialPlayerStacks players = Map.fromList $ [ (PlayerId i, mempty) | i <- [0 .. unPlayerCount players - 1] ]

    -- * Starting the game
    mkInitialState :: InitialData -> SkullsState PlacingCards
    mkInitialState d
      = SkullsPlacingCardsState $
              StateData
                { sdCurrentPlayer = d.idStartingPlayer
                , sdTotalPlayers  = d.idPlayers
                , sdPlayerStacks  = initialPlayerStacks d.idPlayers
                , sdWinCounts     = mempty
                , sdLossCounts    = mempty
                }

    -- * Card played
    mkCardPlayed :: StateData -> TurnData -> SkullsState PlacingCards
    mkCardPlayed stateData turnData =
      let newStacks = insertWith (++) (turnData.tdPlayerId) [turnData.tdCard] (stateData.sdPlayerStacks)
        in SkullsPlacingCardsState $
          (advancePlayer stateData) { sdPlayerStacks = newStacks }

    -- * Starting the betting
    mkFirstBetMade :: StateData -> BetData -> SkullsState Betting
    mkFirstBetMade stateData betData = SkullsBettingState
        (BetStateData
          { bsdHighestBet = betData
          , bsdPlayersBets = Map.singleton stateData.sdCurrentPlayer (Right betData)
          }
        )
        (advancePlayer stateData)

    -- * Raising the bet
    mkRaisingBet :: BetStateData -> StateData -> BetData -> SkullsState Betting
    mkRaisingBet betStateData stateData betData = SkullsBettingState
        (betStateData
          { bsdPlayersBets = Map.insert stateData.sdCurrentPlayer (Right betData) betStateData.bsdPlayersBets
          , bsdHighestBet = betData
          }
        )
        (advancePlayer stateData)

    -- * Passing
    mkPassBet :: BetStateData -> StateData -> PassData -> SkullsState Betting
    mkPassBet betStateData stateData passData = SkullsBettingState
        (betStateData
          { bsdPlayersBets = Map.insert stateData.sdCurrentPlayer (Left passData) betStateData.bsdPlayersBets
          }
        )
        (advancePlayer stateData)

    -- * Resolving the bet
    mkResolvingBet :: StateData -> BetData -> SkullsState ResolvingBet
    mkResolvingBet stateData newBet =
      -- In betting, we still move around Clockwise
      let newState = advancePlayer stateData
      in SkullsResolvingBetState
                  (ResolvingBetStateData
                    { rbsdFlowersToPickUp = newBet.bdFlowers
                    , rbsdCurrentFlowersPickedUp = 0
                    , rbsdPlayerId = newBet.bdPlayerId -- The player who made the highest bet
                    })
                  newState

attemptPickupFrom :: PlayerId -> Int -> StateData -> Event
attemptPickupFrom playerId flowers stateData =
  let cards  = countStackOfPlayer playerId stateData
      toTake = min flowers cards
      -- TODO: The logic here isn't right. This doesn't correctly calculate if
      -- we won our not!
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
                     then WonGame
                     else WonRound
            -- If we do, keep picking up.
            else PickedUp toTake
        --
        -- ** We failed; we hit a skull.
        Left _  -> FailedBet


-- * Rendering

printMermaid :: IO ()
printMermaid = putStrLn $ unpack t
    where
      Mermaid t = renderUntypedGraph (machineAsGraph m)
      m :: StateMachineT Identity Command GameResult
      m = Basic skullsMachine
