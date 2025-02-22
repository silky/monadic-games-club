module Game.Skulls.Generated.Decoder exposing (..)

import Iso8601 as Iso
import Json.Decode as D exposing (..)
import Json.Decode.Pipeline as D exposing (required)

import Game.Skulls.Generated.ElmStreet exposing (..)
import Game.Skulls.Generated.Types as T


decodeSuit : Decoder T.Suit
decodeSuit = elmStreetDecodeEnum T.readSuit

decodeCard : Decoder T.Card
decodeCard =
    let decide : String -> Decoder T.Card
        decide x = case x of
            "Flower" -> D.field "contents" <| D.map T.Flower decodeSuit
            "Skull" -> D.field "contents" <| D.map T.Skull decodeSuit
            c -> D.fail <| "Card doesn't have such constructor: " ++ c
    in D.andThen decide (D.field "tag" D.string)

decodePlayerId : Decoder T.PlayerId
decodePlayerId = D.map T.PlayerId D.int

decodeBetData : Decoder T.BetData
decodeBetData = D.succeed T.BetData
    |> required "flowers" D.int
    |> required "playerId" decodePlayerId

decodePlayerCount : Decoder T.PlayerCount
decodePlayerCount = D.map T.PlayerCount D.int

decodeInitialData : Decoder T.InitialData
decodeInitialData = D.succeed T.InitialData
    |> required "players" decodePlayerCount
    |> required "startingPlayer" decodePlayerId

decodeTurnData : Decoder T.TurnData
decodeTurnData = D.succeed T.TurnData
    |> required "playerId" decodePlayerId
    |> required "card" decodeCard

decodePassData : Decoder T.PassData
decodePassData = D.succeed T.PassData
    |> required "playerId" decodePlayerId

decodeCommand : Decoder T.Command
decodeCommand =
    let decide : String -> Decoder T.Command
        decide x = case x of
            "StartGame" -> D.field "contents" <| D.map T.StartGame decodeInitialData
            "PlayCard" -> D.field "contents" <| D.map T.PlayCard decodeTurnData
            "MakeBet" -> D.field "contents" <| D.map T.MakeBet decodeBetData
            "PassBet" -> D.field "contents" <| D.map T.PassBet decodePassData
            "PickUpMyStack" -> D.succeed T.PickUpMyStack
            "PickUpFrom" -> D.field "contents" <| D.map T.PickUpFrom decodePlayerId
            c -> D.fail <| "Command doesn't have such constructor: " ++ c
    in D.andThen decide (D.field "tag" D.string)

decodeEvent : Decoder T.Event
decodeEvent =
    let decide : String -> Decoder T.Event
        decide x = case x of
            "GameStarted" -> D.field "contents" <| D.map T.GameStarted decodeInitialData
            "CardPlayed" -> D.field "contents" <| D.map T.CardPlayed decodeTurnData
            "BetMade" -> D.field "contents" <| D.map T.BetMade decodeBetData
            "BetPassed" -> D.succeed T.BetPassed
            "PickedUp" -> D.field "contents" <| D.map T.PickedUp D.int
            "WonRound" -> D.succeed T.WonRound
            "WonGame" -> D.succeed T.WonGame
            "FailedBet" -> D.succeed T.FailedBet
            c -> D.fail <| "Event doesn't have such constructor: " ++ c
    in D.andThen decide (D.field "tag" D.string)

decodeStateData : Decoder T.StateData
decodeStateData = D.succeed T.StateData
    |> required "currentPlayer" decodePlayerId
    |> required "playerStacks" (D.list (elmStreetDecodePair decodePlayerId (D.list decodeCard)))
    |> required "winCounts" (D.list (elmStreetDecodePair decodePlayerId D.int))
    |> required "lossCounts" (D.list (elmStreetDecodePair decodePlayerId D.int))
    |> required "totalPlayers" decodePlayerCount

decodeHitSkull : Decoder T.HitSkull
decodeHitSkull = elmStreetDecodeEnum T.readHitSkull

decodeBetStateData : Decoder T.BetStateData
decodeBetStateData = D.succeed T.BetStateData
    |> required "highestBet" decodeBetData
    |> required "playersBets" (D.list (elmStreetDecodePair decodePlayerId (nullable decodeBetData)))

decodeResolvingBetStateData : Decoder T.ResolvingBetStateData
decodeResolvingBetStateData = D.succeed T.ResolvingBetStateData
    |> required "flowersToPickUp" D.int
    |> required "currentFlowersPickedUp" D.int
    |> required "playerId" decodePlayerId

decodeGameError : Decoder T.GameError
decodeGameError = elmStreetDecodeEnum T.readGameError

decodeStateBlob : Decoder T.StateBlob
decodeStateBlob =
    let decide : String -> Decoder T.StateBlob
        decide x = case x of
            "BlobInitialState" -> D.succeed T.BlobInitialState
            "BlobPlacingCardsState" -> D.field "contents" <| D.map T.BlobPlacingCardsState decodeStateData
            "BlobBettingState" -> D.field "contents" <| D.map2 T.BlobBettingState (D.index 0 decodeBetStateData) (D.index 1 decodeStateData)
            "BlobResolvingBetState" -> D.field "contents" <| D.map2 T.BlobResolvingBetState (D.index 0 decodeResolvingBetStateData) (D.index 1 decodeStateData)
            "BlobGameOverState" -> D.field "contents" <| D.map T.BlobGameOverState decodePlayerId
            c -> D.fail <| "StateBlob doesn't have such constructor: " ++ c
    in D.andThen decide (D.field "tag" D.string)

decodeGameResult : Decoder T.GameResult
decodeGameResult = D.map T.GameResult (elmStreetDecodeEither decodeGameError (elmStreetDecodePair decodeEvent decodeStateBlob))
