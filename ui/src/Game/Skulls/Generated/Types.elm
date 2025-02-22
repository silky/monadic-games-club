module Game.Skulls.Generated.Types exposing (..)

import Time exposing (Posix)
import Json.Decode exposing (Value)


type Suit
    = Red
    | Green
    | Blue
    | Orange
    | Pink
    | Black

showSuit : Suit -> String
showSuit x = case x of
    Red -> "Red"
    Green -> "Green"
    Blue -> "Blue"
    Orange -> "Orange"
    Pink -> "Pink"
    Black -> "Black"

readSuit : String -> Maybe Suit
readSuit x = case x of
    "Red" -> Just Red
    "Green" -> Just Green
    "Blue" -> Just Blue
    "Orange" -> Just Orange
    "Pink" -> Just Pink
    "Black" -> Just Black
    _ -> Nothing

universeSuit : List Suit
universeSuit = [Red, Green, Blue, Orange, Pink, Black]

type Card
    = Flower Suit
    | Skull Suit

type PlayerId
    = PlayerId Int

unPlayerId : PlayerId -> Int
unPlayerId (PlayerId x) = x

type alias BetData =
    { flowers : Int
    , playerId : PlayerId
    }

type PlayerCount
    = PlayerCount Int

unPlayerCount : PlayerCount -> Int
unPlayerCount (PlayerCount x) = x

type alias InitialData =
    { players : PlayerCount
    , startingPlayer : PlayerId
    }

type alias TurnData =
    { playerId : PlayerId
    , card : Card
    }

type alias PassData =
    { playerId : PlayerId
    }

type Command
    = StartGame InitialData
    | PlayCard TurnData
    | MakeBet BetData
    | PassBet PassData
    | PickUpMyStack
    | PickUpFrom PlayerId

type Event
    = GameStarted InitialData
    | CardPlayed TurnData
    | BetMade BetData
    | BetPassed
    | PickedUp Int
    | WonRound
    | WonGame
    | FailedBet

type alias StateData =
    { currentPlayer : PlayerId
    , playerStacks : List ((PlayerId, List Card))
    , winCounts : List ((PlayerId, Int))
    , lossCounts : List ((PlayerId, Int))
    , totalPlayers : PlayerCount
    }

type HitSkull
    = HitSkull

showHitSkull : HitSkull -> String
showHitSkull x = case x of
    HitSkull -> "HitSkull"

readHitSkull : String -> Maybe HitSkull
readHitSkull x = case x of
    "HitSkull" -> Just HitSkull
    _ -> Nothing

universeHitSkull : List HitSkull
universeHitSkull = [HitSkull]

type alias BetStateData =
    { highestBet : BetData
    , playersBets : List ((PlayerId, Maybe BetData))
    }

type alias ResolvingBetStateData =
    { flowersToPickUp : Int
    , currentFlowersPickedUp : Int
    , playerId : PlayerId
    }

type GameError
    = TooFewPlayers
    | GameAlreadyStarted
    | GameNotStarted
    | CantPlaceWhileBetting
    | CantPickUpNow
    | NoMoreCardsToPlay
    | PlayedYourSkull
    | BetMustBeHigher
    | CantPassNow
    | BetTooLarge
    | CanOnlyResolveBetNow
    | PickedUpSkull
    | NotYourTurn
    | GameIsOver

showGameError : GameError -> String
showGameError x = case x of
    TooFewPlayers -> "TooFewPlayers"
    GameAlreadyStarted -> "GameAlreadyStarted"
    GameNotStarted -> "GameNotStarted"
    CantPlaceWhileBetting -> "CantPlaceWhileBetting"
    CantPickUpNow -> "CantPickUpNow"
    NoMoreCardsToPlay -> "NoMoreCardsToPlay"
    PlayedYourSkull -> "PlayedYourSkull"
    BetMustBeHigher -> "BetMustBeHigher"
    CantPassNow -> "CantPassNow"
    BetTooLarge -> "BetTooLarge"
    CanOnlyResolveBetNow -> "CanOnlyResolveBetNow"
    PickedUpSkull -> "PickedUpSkull"
    NotYourTurn -> "NotYourTurn"
    GameIsOver -> "GameIsOver"

readGameError : String -> Maybe GameError
readGameError x = case x of
    "TooFewPlayers" -> Just TooFewPlayers
    "GameAlreadyStarted" -> Just GameAlreadyStarted
    "GameNotStarted" -> Just GameNotStarted
    "CantPlaceWhileBetting" -> Just CantPlaceWhileBetting
    "CantPickUpNow" -> Just CantPickUpNow
    "NoMoreCardsToPlay" -> Just NoMoreCardsToPlay
    "PlayedYourSkull" -> Just PlayedYourSkull
    "BetMustBeHigher" -> Just BetMustBeHigher
    "CantPassNow" -> Just CantPassNow
    "BetTooLarge" -> Just BetTooLarge
    "CanOnlyResolveBetNow" -> Just CanOnlyResolveBetNow
    "PickedUpSkull" -> Just PickedUpSkull
    "NotYourTurn" -> Just NotYourTurn
    "GameIsOver" -> Just GameIsOver
    _ -> Nothing

universeGameError : List GameError
universeGameError = [ TooFewPlayers
                    , GameAlreadyStarted
                    , GameNotStarted
                    , CantPlaceWhileBetting
                    , CantPickUpNow
                    , NoMoreCardsToPlay
                    , PlayedYourSkull
                    , BetMustBeHigher
                    , CantPassNow
                    , BetTooLarge
                    , CanOnlyResolveBetNow
                    , PickedUpSkull
                    , NotYourTurn
                    , GameIsOver ]

type StateBlob
    = BlobInitialState
    | BlobPlacingCardsState StateData
    | BlobBettingState BetStateData StateData
    | BlobResolvingBetState ResolvingBetStateData StateData
    | BlobGameOverState PlayerId
