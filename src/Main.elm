module Main exposing (Msg(..), main, update, view)

import Browser
import Browser.Dom
import Browser.Events
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Time


type alias Model =
    { goalX : Int
    , goalY : Int
    , hiScore : Int
    , playerHeading : Action
    , playerX : Int
    , playerY : Int
    , score : Int
    , state : GameState
    , viewportHeight : Int
    , viewportWidth : Int
    }


type Action
    = Up
    | Down
    | Left
    | Right
    | Other


type GameState
    = Playing
    | Complete


type Modifier
    = Normal
    | Fast


type Msg
    = GetViewport Browser.Dom.Viewport
    | Nothing
    | Restart
    | UpdateGoal ( Int, Int )
    | UpdateInput Action Modifier
    | UpdateTick Int
    | UpdateViewport Int Int



-- INIT


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


initialModel hiScore =
    { goalX = -999
    , goalY = -999
    , hiScore = hiScore
    , playerHeading = Right
    , playerX = playerWidth // 2
    , playerY = playerHeight // 2
    , score = 100000
    , state = Playing
    , viewportHeight = 0
    , viewportWidth = 0
    }


initialTask : () -> Cmd Msg
initialTask _ =
    Task.perform GetViewport Browser.Dom.getViewport


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel 0
    , initialTask ()
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetViewport viewport ->
            let
                viewportHeight =
                    round viewport.viewport.height

                viewportWidth =
                    round viewport.viewport.width
            in
            ( { model
                | viewportHeight = viewportHeight
                , viewportWidth = viewportWidth
              }
            , Random.generate UpdateGoal (Random.pair (Random.int 0 (viewportWidth - goalWidth)) (Random.int 0 (viewportHeight - goalHeight)))
            )

        Nothing ->
            ( model, Cmd.none )

        Restart ->
            ( initialModel <| Basics.max model.score model.hiScore, initialTask () )

        UpdateGoal points ->
            ( { model | goalX = Tuple.first points, goalY = Tuple.second points }, Cmd.none )

        UpdateTick score ->
            ( { model | score = Basics.max (model.score - score) 0, state = getState model }, Cmd.none )

        UpdateViewport innerHeight innerWidth ->
            ( { model
                | viewportHeight = innerHeight
                , viewportWidth = innerWidth
              }
            , Cmd.none
            )

        UpdateInput key modifier ->
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
            ( Debug.log "model"
                { model
                    | playerHeading = Up
                    , playerY = Basics.max (model.playerY - offset) (playerHeight // 2)
                }
            , Cmd.none
            )

        Down ->
            ( Debug.log "model"
                { model
                    | playerHeading = Down
                    , playerY = Basics.min (model.playerY + offset) (model.viewportHeight - playerHeight)
                }
            , Cmd.none
            )

        Left ->
            ( Debug.log "model"
                { model
                    | playerHeading = Left
                    , playerX = Basics.max (model.playerX - offset) moveDistance
                }
            , Cmd.none
            )

        Right ->
            ( Debug.log "model"
                { model
                    | playerHeading = Right
                    , playerX = Basics.min (model.playerX + offset) (model.viewportWidth - (playerWidth // 2))
                }
            , Cmd.none
            )

        Other ->
            ( model, Cmd.none )


getState : Model -> GameState
getState model =
    if inGoal model then
        Complete

    else
        Playing


inGoal : Model -> Bool
inGoal model =
    if (model.playerX >= model.goalX && model.playerX <= (model.goalX + goalWidth)) && (model.playerY >= model.goalY && model.playerY <= (model.goalY + goalHeight)) then
        True

    else
        False



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ keyboardSubscription model.state
        , updateSubscription model.state
        , viewportSubscription
        ]


updateSubscription : GameState -> Sub Msg
updateSubscription state =
    if state == Complete then
        Sub.none

    else
        Time.every 16 (\_ -> UpdateTick 100)



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ scoreboard model.score model.hiScore
        , div
            [ Html.Attributes.style "position" "relative" ]
            [ completeMessage model.state
            , player model.playerHeading model.playerX model.playerY
            , goal model.goalX model.goalY
            ]
        ]



-- INPUT


keyboardDecoder : Decode.Decoder Msg
keyboardDecoder =
    Decode.map keyToAction (Decode.field "key" Decode.string)


restartDecoder : Decode.Decoder Msg
restartDecoder =
    Decode.map
        (\key ->
            if key == "r" then
                Restart

            else
                Nothing
        )
        (Decode.field "key" Decode.string)


keyToAction : String -> Msg
keyToAction key =
    case key of
        "W" ->
            UpdateInput Up Fast

        "S" ->
            UpdateInput Down Fast

        "A" ->
            UpdateInput Left Fast

        "D" ->
            UpdateInput Right Fast

        "w" ->
            UpdateInput Up Normal

        "s" ->
            UpdateInput Down Normal

        "a" ->
            UpdateInput Left Normal

        "d" ->
            UpdateInput Right Normal

        _ ->
            Nothing


keyboardSubscription : GameState -> Sub Msg
keyboardSubscription state =
    if state == Complete then
        Browser.Events.onKeyPress restartDecoder

    else
        Browser.Events.onKeyPress keyboardDecoder



-- GRAPHICS


viewportSubscription : Sub Msg
viewportSubscription =
    Browser.Events.onResize updateViewport


updateViewport : Int -> Int -> Msg
updateViewport innerWidth innerHeight =
    UpdateViewport innerHeight innerWidth


goalHeight =
    150


goalWidth =
    150


goalArea =
    goalHeight * goalWidth


playerHeight =
    100


playerWidth =
    50


scoreboardHeight =
    50


completeMessage : GameState -> Html Msg
completeMessage state =
    if state == Complete then
        div []
            [ div
                [ Html.Attributes.style "text-align" "center"
                , Html.Attributes.style "font-family" "sans-serif"
                , Html.Attributes.style "font-size" "32px"
                , Html.Attributes.style "color" "green"
                ]
                [ Html.text "Winner!" ]
            , div
                [ Html.Attributes.style "text-align" "center"
                , Html.Attributes.style "font-family" "sans-serif"
                , Html.Attributes.style "font-size" "16px"
                ]
                [ Html.text "Press r to restart" ]
            ]

    else
        Html.text ""


goal : Int -> Int -> Html Msg
goal x y =
    div
        [ Html.Attributes.style "width" (numberToPixels goalWidth)
        , Html.Attributes.style "height" (numberToPixels goalHeight)
        , Html.Attributes.style "background-color" "green"
        , Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "left" (numberToPixels x)
        , Html.Attributes.style "top" (numberToPixels y)
        , Html.Attributes.style "z-index" "-1"
        ]
        []


scoreboard : Int -> Int -> Html Msg
scoreboard score hiScore =
    div
        [ Html.Attributes.style "width" "100%"
        , Html.Attributes.style "height" (numberToPixels scoreboardHeight)
        , Html.Attributes.style "text-align" "center"
        , Html.Attributes.style "font-family" "sans-serif"
        ]
        [ div [] [ Html.text ("Score:" ++ " " ++ String.fromInt score) ]
        , div [] [ Html.text ("Hi-Score:" ++ " " ++ String.fromInt hiScore) ]
        ]


player : Action -> Int -> Int -> Html Msg
player heading x y =
    div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "left" (numberToPixels x)
        , Html.Attributes.style "top" (numberToPixels y)
        , Html.Attributes.style "transform" "translate(-50%,-50%)"
        ]
        [ svg
            [ width <| numberToPixels playerWidth
            , height <| numberToPixels playerHeight
            ]
            [ rect
                [ fill "blue"
                , width <| numberToPixels playerWidth
                , height <| numberToPixels playerHeight
                ]
                []
            , chevron heading
            ]
        ]


chevron : Action -> Html Msg
chevron heading =
    let
        iconHeight =
            22

        iconWidth =
            16

        icon : Int -> Int -> String -> Html Msg
        icon xPos yPos transformation =
            svg
                [ fill "white"
                , overflow "visible"
                , x (String.fromInt xPos)
                , y (String.fromInt yPos)
                ]
                [ Svg.path
                    [ d "m 4 1 L 10 1 L 20 12 L 10 23 L 4 23 L 14 12 Z"
                    , fill "white"
                    , Html.Attributes.style "transform" transformation
                    ]
                    []
                ]
    in
    case heading of
        Up ->
            icon (playerWidth // 2 - (iconHeight // 2)) (playerHeight // 2 + (iconWidth // 2)) "rotate(-90deg)"

        Down ->
            icon (playerWidth // 2 + (iconHeight // 2)) (playerHeight // 2 - (iconWidth // 2)) "rotate(90deg)"

        Left ->
            icon (playerWidth // 2 + (iconWidth // 2)) (playerHeight // 2 + (iconHeight // 2)) "rotate(180deg)"

        _ ->
            icon (playerWidth // 2 - (iconWidth // 2)) (playerHeight // 2 - (iconHeight // 2)) ""


numberToPixels : Int -> String
numberToPixels coordinate =
    String.fromInt coordinate ++ "px"
