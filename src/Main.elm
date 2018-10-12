module Main exposing (Msg(..), main, update, view)

import Browser
import Browser.Dom
import Browser.Events
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task


type alias Model =
    { playerX : Int
    , playerY : Int
    , viewportHeight : Int
    , viewportWidth : Int
    }


type Action
    = Up
    | Down
    | Left
    | Right
    | Other


type Modifier
    = Normal
    | Fast


type Msg
    = GetViewport Browser.Dom.Viewport
    | UpdateViewport Int Int
    | Update Action Modifier



-- INIT


main =
    Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { playerX = 0, playerY = 0, viewportHeight = 0, viewportWidth = 0 }, Task.perform GetViewport Browser.Dom.getViewport )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetViewport viewport ->
            ( { model | viewportHeight = round viewport.viewport.height, viewportWidth = round viewport.viewport.width }, Cmd.none )

        UpdateViewport innerHeight innerWidth ->
            ( { model | viewportHeight = innerHeight, viewportWidth = innerWidth }, Cmd.none )

        Update key modifier ->
            updateKey key modifier model


updateKey : Action -> Modifier -> Model -> ( Model, Cmd Msg )
updateKey key modifier model =
    let
        moveDistance =
            10

        offset =
            if modifier == Fast then
                moveDistance * 2

            else
                moveDistance
    in
    case key of
        Up ->
            ( Debug.log "model" { model | playerY = Basics.max (model.playerY - offset) 0 }, Cmd.none )

        Down ->
            ( Debug.log "model" { model | playerY = Basics.min (model.playerY + offset) (model.viewportHeight - playerHeight) }, Cmd.none )

        Left ->
            ( Debug.log "model" { model | playerX = Basics.max (model.playerX - offset) 0 }, Cmd.none )

        Right ->
            ( Debug.log "model" { model | playerX = Basics.min (model.playerX + offset) (model.viewportWidth - playerWidth) }, Cmd.none )

        Other ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ keyboardSubscription
        , viewportSubscription
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ player model.playerX model.playerY
        ]



-- INPUT


keyboardDecoder : Decode.Decoder Msg
keyboardDecoder =
    Decode.map keyToAction (Decode.field "key" Decode.string)


keyToAction : String -> Msg
keyToAction key =
    case key of
        "W" ->
            Update Up Fast

        "S" ->
            Update Down Fast

        "A" ->
            Update Left Fast

        "D" ->
            Update Right Fast

        "w" ->
            Update Up Normal

        "s" ->
            Update Down Normal

        "a" ->
            Update Left Normal

        "d" ->
            Update Right Normal

        _ ->
            Update Other Normal


keyboardSubscription : Sub Msg
keyboardSubscription =
    Browser.Events.onKeyPress keyboardDecoder



-- GRAPHICS


viewportSubscription : Sub Msg
viewportSubscription =
    Browser.Events.onResize updateViewport


updateViewport : Int -> Int -> Msg
updateViewport innerWidth innerHeight =
    UpdateViewport innerHeight innerWidth


playerHeight =
    100


playerWidth =
    50


player : Int -> Int -> Html Msg
player x y =
    let
        playerHeightStr =
            String.fromInt playerHeight

        playerWidthStr =
            String.fromInt playerWidth
    in
    div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "left" (coordinateToPixels x)
        , Html.Attributes.style "top" (coordinateToPixels y)
        ]
        [ svg
            [ width playerWidthStr
            , height playerHeightStr
            , viewBox ("0 0" ++ " " ++ playerWidthStr ++ " " ++ playerHeightStr)
            ]
            [ rect
                [ fill "blue"
                , width playerWidthStr
                , height playerHeightStr
                ]
                []
            ]
        ]


coordinateToPixels : Int -> String
coordinateToPixels coordinate =
    String.fromInt coordinate ++ "px"
