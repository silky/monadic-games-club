port module Pages.Skulls exposing (Model, Msg, page)

import Effect exposing (Effect)
import Route exposing (Route)
import Html exposing (Html, text)
import Page exposing (Page)
import Shared
import View exposing (View)
import Json.Encode
import Game.Skulls.Generated.Types


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
    {}


init : () -> ( Model, Effect Msg )
init () =
    ( {}
    , Effect.none
    )



-- * Update

type Msg
    = NoOp
    | SendMessage Json.Encode.Value


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )

        SendMessage v -> ( model, Effect.sendCmd <| sendMessage v )



-- * Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- * View

view : Model -> View Msg
view model =
    { title = "Pages.Skulls"
    , body = body model
    }

body : Model -> List ( Html Msg )
body model = [ text "X" ]
