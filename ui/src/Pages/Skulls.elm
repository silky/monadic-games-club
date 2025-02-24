port module Pages.Skulls exposing (Model, Msg, page)

import Effect exposing (Effect)
import Route exposing (Route)
import Html exposing
  ( Html , button , text , div
  , h2, h3, p, span, em, input
  )
import Html.Events exposing
  ( onClick, onInput
  )
import Html.Attributes exposing
  ( class, placeholder, type_
  )
import Page exposing (Page)
import Shared
import View exposing (View)
import Json.Encode
import Json.Decode exposing (decodeValue)
import Game.Skulls.Generated.Types exposing (..)
import Game.Skulls.Generated.Encoder as Enc
import Game.Skulls.Generated.Decoder as Dec
import List

page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


-- * Ports

port sendMessage    : Json.Encode.Value          -> Cmd msg
port receiveMessage : (Json.Encode.Value -> msg) -> Sub msg


-- * Init

type alias Model =
    { gameState : StateBlob
    , gameError : Maybe GameError
    , betAmount : Maybe Int
    }

init : () -> ( Model, Effect Msg )
init () =
    ( { gameState = BlobInitialState
    -- ( { gameState = ex
      , gameError = Nothing
      , betAmount = Nothing
      }
    , Effect.none
    )

ex
  = BlobPlacingCardsState <|
      { currentPlayer = PlayerId 1
      , playerStacks  =
          [ (PlayerId 0, [ Flower ])
          , (PlayerId 1, [ ])
          , (PlayerId 2, [ ])
          ]
      , winCounts     = []
      , lossCounts    = []
      , totalPlayers  = PlayerCount 3
      }


-- * Update

type Msg
    = NoOp
    | SendMessage Json.Encode.Value
    | ReceiveMessage Json.Encode.Value
    | UpdateBetAmount String


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
          ( model , Effect.none)

        UpdateBetAmount b ->
          ( { model | betAmount = String.toInt b }, Effect.none )

        SendMessage v ->
          ( model, Effect.sendCmd <| sendMessage v )

        ReceiveMessage v ->
          let gameResult = decodeValue Dec.decodeGameResult v
              newModel = case gameResult of
                Ok r ->
                  case unGameResult r of
                    Ok (_, s) -> { model | gameState = s }
                    Err err -> { model | gameError = Just err }
                Err err -> model
          in ( newModel, Effect.none )


-- * Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ receiveMessage ReceiveMessage ]


-- * View

view : Model -> View Msg
view model =
    { title = "Pages.Skulls"
    , body = body model
    }

body : Model -> List ( Html Msg )
body model =
  [ div [] <| renderGameError model.gameError
  , div [] <| renderGameState model
  ]

renderGameState model =
  case model.gameState of
    BlobInitialState ->
      [ h2 [] [ text "Ready to play ..." ]
      , button
        [ onClick
            <| SendMessage
            <| Enc.encodeCommand
            <| StartGame
            <| { players = PlayerCount 3
              , startingPlayer = PlayerId 0
              }
            ]
        [ text "Start game?" ]
      ]

    BlobPlacingCardsState stateData ->
      [ h2 [] [ text "Placing cards" ]
      , renderPlayers model stateData placingCardMoves
      ]

    BlobBettingState betStateData stateData ->
      [ h2 [] [ text "Bet to resolve!" ]
      , renderPlayers model stateData bettingMoves
      ]

    BlobResolvingBetState resolvingBetStateData stateData ->
      [ h2 [] [ text "Resolving the bet!" ]
      , renderPlayers model stateData (resolvingMoves stateData resolvingBetStateData)
      ]

    BlobGameOverState playerId ->
      [ h2 [] [ text "Game over." ]
      ]

renderPlayers
    : Model
    -> StateData
    -> (Model -> PlayerId -> List (Html Msg))
    -> Html Msg
renderPlayers model stateData renderMoves =
  let
      renderPlayer (p, stack) =
        div [ class "stack" ]
            [ h3 [] [ text <| "Player " ++ playerName p ]
            , div [ class "cards" ] <| List.map renderCard stack
            , div [] <| if p == stateData.currentPlayer
                          then renderMoves model p
                          else []
            ]

      renderCard c =
        case c of
          Skull -> span [ class "card" ] [ text "Skull" ]
          Flower -> span [ class "card" ] [ text "Flower" ]

  in
  div []
      [ div [ class "player-stacks" ] (List.map renderPlayer stateData.playerStacks)
      ]


resolvingMoves stateData rbsd model p =
  let otherPlayers = List.filter (\x -> x /= p)
                        <| List.map PlayerId
                        <| List.range 0 (stateData.totalPlayers.playerCount - 1)
      mkPickupFrom pp =
            div
              -- TODO: Convert into a button that triggers the appropriate
              -- command.
              []
              [ text "Pick up "
              , input [ type_ "text" ] []
              , text " flowers from Player "
              , text <| playerName pp
              ]
      moves =
        [ button
            [ onClick
                <| SendMessage
                <| Enc.encodeCommand
                <| PickUpMyStack
            ]
            [ text "Pick up my cards" ]
        , div
            []
            (List.map mkPickupFrom otherPlayers)
        ]
  in
  if p == rbsd.playerId
  then moves
  else []


bettingMoves model p =
  [ button
      [ onClick
          <| SendMessage
          <| Enc.encodeCommand
          <| PassBet
          <| { playerId = p
             }
      ]
      [ text "Pass" ]
  , betWidget model p
  ]


placingCardMoves model p =
  [ button
      [ onClick
          <| SendMessage
          <| Enc.encodeCommand
          <| PlayCard
          <| { playerId = p
              , card = Skull
              }
      ]
      [ text "Place skull" ]

  , button
      [ onClick
          <| SendMessage
          <| Enc.encodeCommand
          <| PlayCard
          <| { playerId = p
              , card = Flower
              }
      ]
      [ text "Place flower" ]
  , betWidget model p
  ]


betWidget model p =
  div
    [ class "bet" ]
    [ text "I bet ... "
    , input [ type_ "text"
            , placeholder "n"
            , onInput UpdateBetAmount
            ]
            []

    , text " flowers. "
    , case model.betAmount of
        Nothing -> text "Make bet"
        Just f  -> button
                    [ onClick
                        <| SendMessage
                        <| Enc.encodeCommand
                        <| MakeBet
                        <| { playerId = p
                            , flowers  = f
                            }
                    ]
                    [ text "Make bet " ]
      ]


playerName : PlayerId -> String
playerName p = String.fromInt <| 1 + unPlayerId p


renderGameError error =
  case error of
    Nothing -> []
    Just e  -> [ text <| showGameError e ]
