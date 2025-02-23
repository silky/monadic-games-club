port module Pages.Skulls exposing (Model, Msg, page)

import Effect exposing (Effect)
import Route exposing (Route)
import Html exposing
  ( Html
  , button
  , text
  , div
  )
import Html.Events exposing
  ( onClick
  )
import Page exposing (Page)
import Shared
import View exposing (View)
import Json.Encode
import Json.Decode exposing (decodeValue)
import Game.Skulls.Generated.Types exposing (..)
import Game.Skulls.Generated.Encoder as Enc
import Game.Skulls.Generated.Decoder as Dec

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
    }

init : () -> ( Model, Effect Msg )
init () =
    ( { gameState = BlobInitialState
      , gameError = Nothing
      }
    , Effect.none
    )


-- * Update

type Msg
    = NoOp
    | SendMessage Json.Encode.Value
    | ReceiveMessage Json.Encode.Value


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )

        SendMessage v -> ( model, Effect.sendCmd <| sendMessage v )

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
  , div [] <| renderGameState model.gameState
  ]

renderGameState gameState =
  case gameState of
    BlobInitialState ->
      [ button
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
      [ text "Placing cards"
      ]

    BlobBettingState betStateData stateData ->
      []

    BlobResolvingBetState resolvingBetStateData stateData ->
      []

    BlobGameOverState playerId ->
      []

renderGameError error =
  case error of
    Nothing -> []
    Just e  -> [ text <| showGameError e ]
