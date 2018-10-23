module Main exposing (initialModel, keyDownDecoder, keyUpDecoder, main, update)

import Browser
import Browser.Dom
import Browser.Events
import Json.Decode as Decode
import Random
import Task
import Types
import View



-- INIT


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = View.view
        }


initialModel hiScore =
    { enemies = []
    , goal = { x = -999, y = -999 }
    , hiScore = hiScore
    , inputQueue = ( Types.Other, Types.Normal )
    , keysDown = []
    , player =
        { heading = Types.Right
        , x = View.playerWidth // 2
        , y = View.playerHeight // 2
        }
    , score = 100000
    , state = Types.Playing
    , viewportHeight = 0
    , viewportWidth = 0
    }


initialTask : () -> Cmd Types.Msg
initialTask _ =
    Task.perform Types.GetViewport Browser.Dom.getViewport


init : () -> ( Types.Model, Cmd Types.Msg )
init _ =
    ( initialModel 0
    , initialTask ()
    )



-- UPDATE


update : Types.Msg -> Types.Model -> ( Types.Model, Cmd Types.Msg )
update msg model =
    case msg of
        Types.ClearKeyDown action ->
            ( clearKeyDown action model, Cmd.none )

        Types.GetViewport viewport ->
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
            , Cmd.batch
                [ Random.generate Types.UpdateGoal (generateGoal (viewportWidth - View.goalWidth) (viewportHeight - View.goalHeight))
                , Random.generate Types.UpdateEnemies (generateEnemies (viewportWidth - View.enemyWidth) (viewportHeight - View.enemyHeight))
                ]
            )

        Types.Nothing ->
            ( model, Cmd.none )

        Types.Pause ->
            ( { model | state = Types.Paused }, Cmd.none )

        Types.Restart ->
            ( initialModel <| Basics.max model.score model.hiScore, initialTask () )

        Types.Resume ->
            ( { model | state = Types.Playing }, Cmd.none )

        Types.Tick dt ->
            ( { model
                | enemies = List.map (updateEnemyPosition dt model.player) model.enemies
                , player = updatePlayer dt model
                , score = Basics.max (model.score - 100) 0
                , state = getState model
              }
            , Cmd.none
            )

        Types.UpdateEnemies nmes ->
            ( { model | enemies = nmes }, Cmd.none )

        Types.UpdateGoal points ->
            ( { model | goal = { x = Tuple.first points, y = Tuple.second points } }, Cmd.none )

        Types.UpdateInputQueue action modifier ->
            ( { model | keysDown = action :: model.keysDown, inputQueue = ( action, modifier ) }, Cmd.none )

        Types.UpdateViewport innerHeight innerWidth ->
            ( { model
                | viewportHeight = innerHeight
                , viewportWidth = innerWidth
              }
            , Cmd.none
            )


clearKeyDown : Types.Action -> Types.Model -> Types.Model
clearKeyDown action model =
    let
        newKeysDown =
            List.filter (\a -> a /= action) model.keysDown

        lastKey =
            Maybe.withDefault Types.Other (List.head newKeysDown)
    in
    { model | keysDown = newKeysDown, inputQueue = ( lastKey, Tuple.second model.inputQueue ) }


getState : Types.Model -> Types.GameState
getState model =
    if inGoal model then
        Types.Complete

    else if collision model then
        Types.GameOver

    else
        Types.Playing


inGoal : Types.Model -> Bool
inGoal model =
    if
        (model.player.x >= model.goal.x && model.player.x <= (model.goal.x + View.goalWidth))
            && (model.player.y >= model.goal.y && model.player.y <= (model.goal.y + View.goalHeight))
    then
        True

    else
        False


collision : Types.Model -> Bool
collision model =
    List.any (enemyCollision model.player.x model.player.y) model.enemies


enemyCollision : Int -> Int -> Types.Enemy -> Bool
enemyCollision playerX playerY nme =
    nme.x == playerX && nme.y == playerY


generateGoal : Int -> Int -> Random.Generator ( Int, Int )
generateGoal xBound yBound =
    Random.pair
        (Random.int 0 xBound)
        (Random.int 0 yBound)


generateEnemies : Int -> Int -> Random.Generator Types.Enemies
generateEnemies xBound yBound =
    Random.int 2 4
        |> Random.andThen
            (\num -> Random.list num (generateEnemy xBound yBound))


generateEnemy : Int -> Int -> Random.Generator Types.Enemy
generateEnemy xBound yBound =
    Random.map3
        (\x y speed -> { speed = speed, x = x, y = y })
        (Random.int 0 xBound)
        (Random.int 0 yBound)
        (Random.float 0.2 0.5)


updatePlayer : Float -> Types.Model -> Types.Player
updatePlayer dt model =
    let
        action =
            Tuple.first model.inputQueue

        modifier =
            Tuple.second model.inputQueue

        moveSpeed =
            0.45

        offset =
            Basics.round
                (if modifier == Types.Fast then
                    (moveSpeed * 1.5) * dt

                 else
                    moveSpeed * dt
                )
    in
    case action of
        Types.Up ->
            { heading = Types.Up
            , x = model.player.x
            , y = Basics.max (model.player.y - offset) (View.playerHeight // 2)
            }

        Types.Down ->
            { heading = Types.Down
            , x = model.player.x
            , y = Basics.min (model.player.y + offset) (model.viewportHeight - View.playerHeight)
            }

        Types.Left ->
            { heading = Types.Left
            , x = Basics.max (model.player.x - offset) (View.playerWidth // 2)
            , y = model.player.y
            }

        Types.Right ->
            { heading = Types.Right
            , x = Basics.min (model.player.x + offset) (model.viewportWidth - (View.playerWidth // 2))
            , y = model.player.y
            }

        _ ->
            { heading = model.player.heading
            , x = model.player.x
            , y = model.player.y
            }


updateEnemyPosition : Float -> Types.Player -> Types.Enemy -> Types.Enemy
updateEnemyPosition dt player enemy =
    { enemy
        | x = findPlayer dt player.x enemy.x enemy.speed
        , y = findPlayer dt player.y enemy.y enemy.speed
    }


findPlayer : Float -> Int -> Int -> Float -> Int
findPlayer dt playerCoordinate enemyCoordinate enemySpeed =
    let
        distance =
            Basics.round (enemySpeed * dt)
    in
    -- Add tolerance for where enemy is on same horizontal / vertical line as player to stop janky animation
    if Basics.abs (enemyCoordinate - playerCoordinate) <= distance then
        playerCoordinate

    else if enemyCoordinate - playerCoordinate > 0 then
        enemyCoordinate - distance

    else
        enemyCoordinate + distance



-- SUBSCRIPTIONS


subscriptions : Types.Model -> Sub Types.Msg
subscriptions model =
    Sub.batch
        ([ tickSubscription model
         , viewportSubscription
         ]
            ++ inputSubscription model.state
        )


inputSubscription : Types.GameState -> List (Sub Types.Msg)
inputSubscription state =
    if state == Types.Playing || state == Types.Paused then
        [ Browser.Events.onKeyDown keyDownDecoder
        , Browser.Events.onKeyUp keyUpDecoder
        , Browser.Events.onKeyPress (pauseDecoder state)
        ]

    else
        [ Browser.Events.onKeyPress restartDecoder ]


keyDownDecoder : Decode.Decoder Types.Msg
keyDownDecoder =
    Decode.map keyDownToAction (Decode.field "key" Decode.string)


keyUpDecoder : Decode.Decoder Types.Msg
keyUpDecoder =
    Decode.map keyUpToAction (Decode.field "key" Decode.string)


keyToAction : String -> Types.Action
keyToAction key =
    case String.toLower key of
        "w" ->
            Types.Up

        "s" ->
            Types.Down

        "a" ->
            Types.Left

        "d" ->
            Types.Right

        _ ->
            Types.Other


keyUpToAction : String -> Types.Msg
keyUpToAction key =
    if List.any (\k -> String.toLower key == k) [ "w", "s", "a", "d" ] then
        Types.ClearKeyDown (keyToAction key)

    else
        Types.Nothing


keyDownToAction : String -> Types.Msg
keyDownToAction key =
    if List.any (\k -> k == key) [ "W", "S", "A", "D" ] then
        Types.UpdateInputQueue (keyToAction key) Types.Fast

    else if List.any (\k -> k == key) [ "w", "s", "a", "d" ] then
        Types.UpdateInputQueue (keyToAction key) Types.Normal

    else
        Types.Nothing


pauseDecoder : Types.GameState -> Decode.Decoder Types.Msg
pauseDecoder state =
    Decode.map
        (\key ->
            if state == Types.Playing && key == "p" then
                Types.Pause

            else if state == Types.Paused && key == "p" then
                Types.Resume

            else
                Types.Nothing
        )
        (Decode.field "key" Decode.string)


restartDecoder : Decode.Decoder Types.Msg
restartDecoder =
    Decode.map
        (\key ->
            if key == "r" then
                Types.Restart

            else
                Types.Nothing
        )
        (Decode.field "key" Decode.string)


tickSubscription : Types.Model -> Sub Types.Msg
tickSubscription model =
    if model.state == Types.Playing then
        Browser.Events.onAnimationFrameDelta (\dt -> Types.Tick dt)

    else
        Sub.none


viewportSubscription : Sub Types.Msg
viewportSubscription =
    Browser.Events.onResize (\innerWidth innerHeight -> Types.UpdateViewport innerHeight innerWidth)
