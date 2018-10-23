module MainTests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as Decode
import Main
import Test exposing (..)
import Types


keyEvent : String -> String
keyEvent key =
    "{ \"key\": \"" ++ key ++ "\" }"


decodeKeyDownEvent : String -> Types.Msg
decodeKeyDownEvent event =
    Result.withDefault
        Types.Nothing
        (Decode.decodeString Main.keyDownDecoder event)


decodeKeyUpEvent : String -> Types.Msg
decodeKeyUpEvent event =
    Result.withDefault
        Types.Nothing
        (Decode.decodeString Main.keyUpDecoder event)


model =
    Main.initialModel 0


suite : Test
suite =
    describe "The Main module"
        [ describe "Update"
            [ describe "Main.update"
                [ describe "Types.Nothing"
                    [ test "leaves the model untouched" <|
                        \_ ->
                            Expect.equal
                                ( model, Cmd.none )
                                (Main.update Types.Nothing model)
                    ]
                , describe "Types.Pause"
                    [ test "sets the game state to paused" <|
                        \_ ->
                            Expect.equal
                                ( { model | state = Types.Paused }, Cmd.none )
                                (Main.update Types.Pause model)
                    ]
                , describe "Types.Resume"
                    [ test "sets the game state to playing" <|
                        \_ ->
                            Expect.equal
                                ( { model | state = Types.Playing }, Cmd.none )
                                (Main.update Types.Resume model)
                    ]
                , describe "Types.UpdateEnemies"
                    [ test "updates the enemies in the model" <|
                        let
                            enemies =
                                [ { speed = 0.3, x = 10, y = 10 } ]
                        in
                        \_ ->
                            Expect.equal
                                ( { model | enemies = enemies }, Cmd.none )
                                (Main.update (Types.UpdateEnemies enemies) model)
                    ]
                , describe "Types.UpdateGoal"
                    [ test "updates the goal in the model" <|
                        \_ ->
                            Expect.equal
                                ( { model | goal = { x = 5, y = 5 } }, Cmd.none )
                                (Main.update (Types.UpdateGoal ( 5, 5 )) model)
                    ]
                , describe "Types.UpdateInputQueue"
                    [ test "updates the keys down and input queue in the state" <|
                        \_ ->
                            Expect.equal
                                ( { model | keysDown = [ Types.Up, Types.Down ], inputQueue = ( Types.Up, Types.Fast ) }, Cmd.none )
                                (Main.update (Types.UpdateInputQueue Types.Up Types.Fast) { model | keysDown = [ Types.Down ] })
                    ]
                , describe "Types.UpdateViewport"
                    [ test "updates the viewport in the model" <|
                        \_ ->
                            Expect.equal
                                ( { model | viewportHeight = 5, viewportWidth = 10 }, Cmd.none )
                                (Main.update (Types.UpdateViewport 5 10) model)
                    ]
                ]
            ]
        , describe "Subscriptions"
            [ describe "Main.keyDownDecoder"
                [ test "updates input queue with Up Fast when W is pressed" <|
                    \_ ->
                        Expect.equal
                            (Types.UpdateInputQueue Types.Up Types.Fast)
                            (decodeKeyDownEvent <| keyEvent "W")
                , test "updates input queue with Down Fast when S is pressed" <|
                    \_ ->
                        Expect.equal
                            (Types.UpdateInputQueue Types.Down Types.Fast)
                            (decodeKeyDownEvent <| keyEvent "S")
                , test "updates input queue with Left Fast when A is pressed" <|
                    \_ ->
                        Expect.equal
                            (Types.UpdateInputQueue Types.Left Types.Fast)
                            (decodeKeyDownEvent <| keyEvent "A")
                , test "updates input queue with Right Fast when D is pressed" <|
                    \_ ->
                        Expect.equal
                            (Types.UpdateInputQueue Types.Right Types.Fast)
                            (decodeKeyDownEvent <| keyEvent "D")
                , test "updates input queue with Up Normal when w is pressed" <|
                    \_ ->
                        Expect.equal
                            (Types.UpdateInputQueue Types.Up Types.Normal)
                            (decodeKeyDownEvent <| keyEvent "w")
                , test "updates input queue with Down Normal when s is pressed" <|
                    \_ ->
                        Expect.equal
                            (Types.UpdateInputQueue Types.Down Types.Normal)
                            (decodeKeyDownEvent <| keyEvent "s")
                , test "updates input queue with Left Normal when a is pressed" <|
                    \_ ->
                        Expect.equal
                            (Types.UpdateInputQueue Types.Left Types.Normal)
                            (decodeKeyDownEvent <| keyEvent "a")
                , test "updates input queue with Right Normal when d is pressed" <|
                    \_ ->
                        Expect.equal
                            (Types.UpdateInputQueue Types.Right Types.Normal)
                            (decodeKeyDownEvent <| keyEvent "d")
                , test "does nothing if non-movement key is pressed" <|
                    \_ ->
                        Expect.equal
                            Types.Nothing
                            (decodeKeyDownEvent <| keyEvent "?")
                ]
            , describe "Main.keyUpDecoder"
                [ test "clears Up action from input queue when w is pressed" <|
                    \_ ->
                        Expect.equal
                            (Types.ClearInputQueue Types.Up)
                            (decodeKeyUpEvent <| keyEvent "w")
                , test "clears Down action from input queue when s is pressed" <|
                    \_ ->
                        Expect.equal
                            (Types.ClearInputQueue Types.Down)
                            (decodeKeyUpEvent <| keyEvent "s")
                , test "clears Left action from input queue when a is pressed" <|
                    \_ ->
                        Expect.equal
                            (Types.ClearInputQueue Types.Left)
                            (decodeKeyUpEvent <| keyEvent "a")
                , test "clears Right action from input queue when d is pressed" <|
                    \_ ->
                        Expect.equal
                            (Types.ClearInputQueue Types.Right)
                            (decodeKeyUpEvent <| keyEvent "d")
                , test "handles speedy movement" <|
                    \_ ->
                        Expect.equal
                            (Types.ClearInputQueue Types.Up)
                            (decodeKeyUpEvent <| keyEvent "W")
                , test "does nothing if non-movement key is pressed" <|
                    \_ ->
                        Expect.equal
                            Types.Nothing
                            (decodeKeyDownEvent <| keyEvent "?")
                ]
            ]
        ]
