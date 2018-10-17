module Types exposing (Action(..), Enemies, Enemy, GameState(..), Goal, Model, Modifier(..), Msg(..), Player)

import Browser.Dom


type alias Model =
    { enemies : Enemies
    , goal : Goal
    , hiScore : Int
    , inputQueue : ( Action, Modifier )
    , keysDown : List Action
    , player : Player
    , score : Int
    , state : GameState
    , viewportHeight : Int
    , viewportWidth : Int
    }


type alias Enemy =
    { x : Int
    , y : Int
    , speed : Float
    }


type alias Goal =
    { x : Int
    , y : Int
    }


type alias Player =
    { x : Int
    , y : Int
    , heading : Action
    }


type alias Enemies =
    List Enemy


type Action
    = Up
    | Down
    | Left
    | Right
    | Other


type GameState
    = Complete
    | GameOver
    | Paused
    | Playing


type Modifier
    = Normal
    | Fast


type Msg
    = ClearInputQueue Action
    | GetViewport Browser.Dom.Viewport
    | Nothing
    | Pause
    | Restart
    | Resume
    | Tick Float
    | UpdateEnemies Enemies
    | UpdateGoal ( Int, Int )
    | UpdateInputQueue Action Modifier
    | UpdateViewport Int Int
