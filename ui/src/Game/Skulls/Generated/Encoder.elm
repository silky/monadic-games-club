module Game.Skulls.Generated.Encoder exposing (..)

import Iso8601 as Iso
import Json.Encode as E exposing (..)

import Game.Skulls.Generated.ElmStreet exposing (..)
import Game.Skulls.Generated.Types as T


encodeSuit : T.Suit -> Value
encodeSuit = E.string << T.showSuit

encodeCard : T.Card -> Value
encodeCard x = E.object <| case x of
    T.Flower x1 -> [("tag", E.string "Flower"), ("contents", encodeSuit x1)]
    T.Skull x1 -> [("tag", E.string "Skull"), ("contents", encodeSuit x1)]

encodePlayerId : T.PlayerId -> Value
encodePlayerId = E.int << T.unPlayerId

encodeBetData : T.BetData -> Value
encodeBetData x = E.object
    [ ("tag", E.string "BetData")
    , ("flowers", E.int x.flowers)
    , ("playerId", encodePlayerId x.playerId)
    ]

encodePlayerCount : T.PlayerCount -> Value
encodePlayerCount = E.int << T.unPlayerCount

encodeInitialData : T.InitialData -> Value
encodeInitialData x = E.object
    [ ("tag", E.string "InitialData")
    , ("players", encodePlayerCount x.players)
    , ("startingPlayer", encodePlayerId x.startingPlayer)
    ]

encodeTurnData : T.TurnData -> Value
encodeTurnData x = E.object
    [ ("tag", E.string "TurnData")
    , ("playerId", encodePlayerId x.playerId)
    , ("card", encodeCard x.card)
    ]

encodePassData : T.PassData -> Value
encodePassData x = E.object
    [ ("tag", E.string "PassData")
    , ("playerId", encodePlayerId x.playerId)
    ]

encodeCommand : T.Command -> Value
encodeCommand x = E.object <| case x of
    T.StartGame x1 -> [("tag", E.string "StartGame"), ("contents", encodeInitialData x1)]
    T.PlayCard x1 -> [("tag", E.string "PlayCard"), ("contents", encodeTurnData x1)]
    T.MakeBet x1 -> [("tag", E.string "MakeBet"), ("contents", encodeBetData x1)]
    T.PassBet x1 -> [("tag", E.string "PassBet"), ("contents", encodePassData x1)]
    T.PickUpMyStack  -> [("tag", E.string "PickUpMyStack"), ("contents", E.list identity [])]
    T.PickUpFrom x1 -> [("tag", E.string "PickUpFrom"), ("contents", encodePlayerId x1)]

encodeEvent : T.Event -> Value
encodeEvent x = E.object <| case x of
    T.GameStarted x1 -> [("tag", E.string "GameStarted"), ("contents", encodeInitialData x1)]
    T.CardPlayed x1 -> [("tag", E.string "CardPlayed"), ("contents", encodeTurnData x1)]
    T.BetMade x1 -> [("tag", E.string "BetMade"), ("contents", encodeBetData x1)]
    T.BetPassed  -> [("tag", E.string "BetPassed"), ("contents", E.list identity [])]
    T.PickedUp x1 -> [("tag", E.string "PickedUp"), ("contents", E.int x1)]
    T.WonRound  -> [("tag", E.string "WonRound"), ("contents", E.list identity [])]
    T.WonGame  -> [("tag", E.string "WonGame"), ("contents", E.list identity [])]
    T.FailedBet  -> [("tag", E.string "FailedBet"), ("contents", E.list identity [])]

encodeStateData : T.StateData -> Value
encodeStateData x = E.object
    [ ("tag", E.string "StateData")
    , ("currentPlayer", encodePlayerId x.currentPlayer)
    , ("playerStacks", (E.list (elmStreetEncodePair encodePlayerId (E.list encodeCard))) x.playerStacks)
    , ("winCounts", (E.list (elmStreetEncodePair encodePlayerId E.int)) x.winCounts)
    , ("lossCounts", (E.list (elmStreetEncodePair encodePlayerId E.int)) x.lossCounts)
    , ("totalPlayers", encodePlayerCount x.totalPlayers)
    ]

encodeHitSkull : T.HitSkull -> Value
encodeHitSkull = E.string << T.showHitSkull

encodeBetStateData : T.BetStateData -> Value
encodeBetStateData x = E.object
    [ ("tag", E.string "BetStateData")
    , ("highestBet", encodeBetData x.highestBet)
    , ("playersBets", (E.list (elmStreetEncodePair encodePlayerId (elmStreetEncodeMaybe encodeBetData))) x.playersBets)
    ]

encodeResolvingBetStateData : T.ResolvingBetStateData -> Value
encodeResolvingBetStateData x = E.object
    [ ("tag", E.string "ResolvingBetStateData")
    , ("flowersToPickUp", E.int x.flowersToPickUp)
    , ("currentFlowersPickedUp", E.int x.currentFlowersPickedUp)
    , ("playerId", encodePlayerId x.playerId)
    ]

encodeGameError : T.GameError -> Value
encodeGameError = E.string << T.showGameError

encodeStateBlob : T.StateBlob -> Value
encodeStateBlob x = E.object <| case x of
    T.BlobInitialState  -> [("tag", E.string "BlobInitialState"), ("contents", E.list identity [])]
    T.BlobPlacingCardsState x1 -> [("tag", E.string "BlobPlacingCardsState"), ("contents", encodeStateData x1)]
    T.BlobBettingState x1 x2 -> [("tag", E.string "BlobBettingState"), ("contents", E.list identity [encodeBetStateData x1, encodeStateData x2])]
    T.BlobResolvingBetState x1 x2 -> [("tag", E.string "BlobResolvingBetState"), ("contents", E.list identity [encodeResolvingBetStateData x1, encodeStateData x2])]
    T.BlobGameOverState x1 -> [("tag", E.string "BlobGameOverState"), ("contents", encodePlayerId x1)]
