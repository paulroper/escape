module Types exposing (Action(..), Enemies, Enemy, GameState(..), Goal, Model, Modifier(..), Msg(..), Player)

import Browser.Dom


type alias Model =
    { enemies : Enemies
    , goal : Goal
    , hiScore : Int
    , inputQueue : ( Action, Modifier )
    , player : Player
    , score : Int
    , state : GameState
    , viewportHeight : Int
    , viewportWidth : Int
    }


type alias Enemy =
    { x : Int
    , y : Int
    , speed : Int
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
    = Playing
    | Complete
    | GameOver


type Modifier
    = Normal
    | Fast


type Msg
    = GetViewport Browser.Dom.Viewport
    | Nothing
    | Restart
    | UpdateEnemies Enemies
    | UpdateGoal ( Int, Int )
    | UpdateInputQueue Action Modifier
    | Tick Float
    | UpdateViewport Int Int
