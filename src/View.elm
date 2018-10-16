module View exposing (enemyHeight, enemyWidth, goalHeight, goalWidth, playerHeight, playerWidth, view)

import Browser.Events
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Types



-- VIEW


view : Types.Model -> Html Types.Msg
view model =
    div []
        [ scoreboard model.score model.hiScore
        , div
            [ Html.Attributes.style "position" "relative" ]
            ([ message model.state
             , player model.player.heading model.player.x model.player.y
             , goal model.goal.x model.goal.y
             ]
                ++ enemies model.enemies
            )
        ]



-- GRAPHICS


enemyHeight =
    25


enemyWidth =
    25


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


message : Types.GameState -> Html Types.Msg
message state =
    if state == Types.Complete then
        div
            [ Html.Attributes.style "padding-top" "50px"
            ]
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

    else if state == Types.GameOver then
        div
            [ Html.Attributes.style "padding-top" "50px"
            ]
            [ div
                [ Html.Attributes.style "text-align" "center"
                , Html.Attributes.style "font-family" "sans-serif"
                , Html.Attributes.style "font-size" "32px"
                , Html.Attributes.style "color" "red"
                ]
                [ Html.text "Game Over!" ]
            , div
                [ Html.Attributes.style "text-align" "center"
                , Html.Attributes.style "font-family" "sans-serif"
                , Html.Attributes.style "font-size" "16px"
                ]
                [ Html.text "Press r to restart" ]
            ]

    else
        Html.text ""


goal : Int -> Int -> Html Types.Msg
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


scoreboard : Int -> Int -> Html Types.Msg
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


enemies : Types.Enemies -> List (Html Types.Msg)
enemies nmes =
    List.map enemy nmes


enemy : Types.Enemy -> Html Types.Msg
enemy nme =
    div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "left" (numberToPixels nme.x)
        , Html.Attributes.style "top" (numberToPixels nme.y)
        , Html.Attributes.style "transform" "translate(-50%,-50%)"
        , Html.Attributes.style "z-index" "1"
        ]
        [ svg
            [ width <| numberToPixels enemyWidth
            , height <| numberToPixels enemyHeight
            ]
            [ circle
                [ fill "red"
                , cx <| numberToPixels (enemyWidth // 2)
                , cy <| numberToPixels (enemyHeight // 2)
                , r <| numberToPixels (enemyWidth // 2)
                ]
                []
            ]
        ]


player : Types.Action -> Int -> Int -> Html Types.Msg
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


chevron : Types.Action -> Html Types.Msg
chevron heading =
    let
        iconHeight =
            22

        iconWidth =
            16

        icon : Int -> Int -> String -> Html Types.Msg
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
        Types.Up ->
            icon (playerWidth // 2 - (iconHeight // 2)) (playerHeight // 2 + (iconWidth // 2)) "rotate(-90deg)"

        Types.Down ->
            icon (playerWidth // 2 + (iconHeight // 2)) (playerHeight // 2 - (iconWidth // 2)) "rotate(90deg)"

        Types.Left ->
            icon (playerWidth // 2 + (iconWidth // 2)) (playerHeight // 2 + (iconHeight // 2)) "rotate(180deg)"

        _ ->
            icon (playerWidth // 2 - (iconWidth // 2)) (playerHeight // 2 - (iconHeight // 2)) ""


numberToPixels : Int -> String
numberToPixels coordinate =
    String.fromInt coordinate ++ "px"
