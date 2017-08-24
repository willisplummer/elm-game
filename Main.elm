module Main exposing (..)

import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Text
import Char
import Time exposing (..)
import Window exposing (..)
import Html exposing (..)
import Keyboard exposing (..)
import Set exposing (Set)
import Task
import AnimationFrame


main : Platform.Program Never GameState Msg
main =
    program
        { init = ( initialGame, initialSizeCmd )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- initialSizeCmd/sizeToMsg technique taken from this answer :
--     https://www.reddit.com/r/elm/comments/4jfo32/getting_the_initial_window_dimensions/d369kw1/
--
-- to this question :
--     https://www.reddit.com/r/elm/comments/4jfo32/getting_the_initial_window_dimensions/


initialSizeCmd : Cmd Msg
initialSizeCmd =
    Task.perform sizeToMsg (Window.size)


sizeToMsg : Window.Size -> Msg
sizeToMsg size =
    WindowResize ( size.width, size.height )


( gameWidth, gameHeight ) =
    ( 600, 400 )
( halfWidth, halfHeight ) =
    ( gameWidth / 2, gameHeight / 2 )



-- MODEL


type alias GameState =
    { keysDown : Set KeyCode
    , windowDimensions : ( Int, Int )
    , objects : List ( Object, Int, Int )
    , player : Player
    }


initialGame : GameState
initialGame =
    { keysDown = Set.empty, windowDimensions = ( 0, 0 ), objects = [], player = initialPlayer }


initialPlayer : Player
initialPlayer =
    { orientation = North
    , position = ( 0, 0 )
    }


type alias Player =
    { orientation : Orientation
    , position : ( Float, Float )
    }


type Orientation
    = North
    | East
    | South
    | West


type Object
    = Bath
    | Sink
    | Bed
    | Stove
    | Microwave
    | Computer


type Direction
    = Up
    | Down
    | Left
    | Right
    | Neutral


type alias KeyInput =
    { space : Bool, direction : Direction, delta : Float }


type Key
    = UpKey
    | DownKey
    | LeftKey
    | RightKey


updateDirection : Key -> Direction
updateDirection key =
    case key of
        UpKey ->
            Up

        DownKey ->
            Down

        LeftKey ->
            Left

        RightKey ->
            Right



-- UPDATE


updateGame : KeyInput -> GameState -> GameState
updateGame { delta, direction } ({ player } as game) =
    { game
        | player = updatePlayer delta direction player
    }


updatePlayer : Time -> Direction -> Player -> Player
updatePlayer t dir ({ position } as player) =
    let
        newPos =
            case dir of
                Right ->
                    Tuple.mapFirst (\x -> x + 200 * t) position

                Left ->
                    Tuple.mapFirst (\x -> x - 200 * t) position

                Up ->
                    Tuple.mapSecond (\x -> x + 200 * t) position

                Down ->
                    Tuple.mapSecond (\x -> x - 200 * t) position

                Neutral ->
                    position
    in
        { player
            | position = newPos
        }


getInput : GameState -> Float -> KeyInput
getInput game delta =
    { space = Set.member (Char.toCode ' ') (game.keysDown)
    , direction =
        if Set.member 37 (game.keysDown) then
            Left
        else if Set.member 38 (game.keysDown) then
            Up
        else if Set.member 39 (game.keysDown) then
            Right
        else if Set.member 40 (game.keysDown) then
            Down
        else
            Neutral
    , delta = inSeconds delta
    }


type Msg
    = KeyDown KeyCode
    | KeyUp KeyCode
    | WindowResize ( Int, Int )
    | Tick Float
    | NoOp


update : Msg -> GameState -> ( GameState, Cmd Msg )
update msg game =
    case msg of
        KeyDown key ->
            ( { game | keysDown = Set.insert key game.keysDown }, Cmd.none )

        KeyUp key ->
            ( { game | keysDown = Set.remove key game.keysDown }, Cmd.none )

        Tick delta ->
            let
                input =
                    getInput game delta
            in
                ( updateGame input game, Cmd.none )

        WindowResize dim ->
            ( { game | windowDimensions = dim }, Cmd.none )

        NoOp ->
            ( game, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : a -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        , Window.resizes sizeToMsg
        , AnimationFrame.diffs Tick
        ]



-- VIEW


view : GameState -> Html Msg
view state =
    div []
        [ div [] [ Html.text (toString state.player.position) ]
        ]
