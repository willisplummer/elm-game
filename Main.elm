module Main exposing (..)

import AnimationFrame
import Char
import Collage exposing (..)
import Color exposing (..)
import Element exposing (..)
import Html exposing (..)
import Keyboard exposing (..)
import Set exposing (Set)
import Task
import Time exposing (..)
import Window exposing (..)


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
    Task.perform sizeToMsg Window.size


sizeToMsg : Window.Size -> Msg
sizeToMsg size =
    WindowResize ( size.width, size.height )



-- MODEL


type alias GameState =
    { keysDown : Set KeyCode
    , windowDimensions : ( Int, Int )
    , objects : List PositionObject
    , player : Player
    }


initialGame : GameState
initialGame =
    { keysDown = Set.empty, windowDimensions = ( 0, 0 ), objects = initialObjects, player = initialPlayer }


initialPlayer : Player
initialPlayer =
    { orientation = North
    , position = ( 0, 0 )
    , size = ( 15, 15 )
    }


type alias Size =
    ( Float, Float )


type alias PositionObject =
    { color : Color, kind : Object, position : ( Float, Float ), size : Size }


initialObjects : List PositionObject
initialObjects =
    [ { color = green, kind = Bed, position = ( gameWidth / 2, gameHeight / 2 ), size = ( 80, 160 ) } ]


type alias Player =
    { orientation : Orientation
    , position : ( Float, Float )
    , size : Size
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



-- UPDATE


updateGame : KeyInput -> GameState -> GameState
updateGame { delta, directions } ({ player } as game) =
    { game
        | player = updatePlayer delta directions player
    }


top : ( Float, Float ) -> Size -> Float
top ( x, y ) ( w, h ) =
    y + h / 2


left : ( Float, Float ) -> Size -> Float
left ( x, _ ) ( w, _ ) =
    x - w / 2


bottom : ( Float, Float ) -> Size -> Float
bottom ( _, y ) ( _, h ) =
    y - h / 2


right : ( Float, Float ) -> Size -> Float
right ( x, _ ) ( w, _ ) =
    x + w / 2


canGo : Player -> List Direction
canGo player =
    let
        upperLimit =
            gameHeight / 2

        lowerLimit =
            -(gameHeight / 2)

        leftLimit =
            -(gameWidth / 2)

        rightLimit =
            gameWidth / 2

        pTop =
            top player.position player.size

        pBottom =
            bottom player.position player.size

        pLeft =
            left player.position player.size

        pRight =
            right player.position player.size

        atTop =
            upperLimit <= pTop

        atBottom =
            lowerLimit >= pBottom

        atLeft =
            leftLimit >= pLeft

        atRight =
            rightLimit <= pRight

        alwaysCan =
            [ Left, Right, Down ]
    in
    if atTop then
        alwaysCan
    else
        alwaysCan ++ [ Up ]


updatePlayer : Time -> List Direction -> Player -> Player
updatePlayer t directions ({ position, orientation } as player) =
    let
        rateOfMovement =
            200

        validDirections =
            List.filter (\x -> List.member x (canGo player)) directions

        updatePosition dir pos =
            case dir of
                Up ->
                    Tuple.mapSecond (\y -> y + rateOfMovement * t) pos

                Down ->
                    Tuple.mapSecond (\y -> y - rateOfMovement * t) pos

                Left ->
                    Tuple.mapFirst (\y -> y - rateOfMovement * t) pos

                Right ->
                    Tuple.mapFirst (\y -> y + rateOfMovement * t) pos

        newPos =
            List.foldl updatePosition position validDirections
    in
    { player
        | position = newPos
    }


type Direction
    = Left
    | Right
    | Up
    | Down


type alias KeyInput =
    { space : Bool, directions : List Direction, delta : Float }


getInput : GameState -> Float -> KeyInput
getInput game delta =
    let
        horizontals =
            if Set.member 37 game.keysDown && Set.member 39 game.keysDown then
                []
            else if Set.member 37 game.keysDown then
                [ Left ]
            else if Set.member 39 game.keysDown then
                [ Right ]
            else
                []

        verticals =
            if Set.member 38 game.keysDown && Set.member 40 game.keysDown then
                []
            else if Set.member 38 game.keysDown then
                [ Up ]
            else if Set.member 40 game.keysDown then
                [ Down ]
            else
                []
    in
    { space = Set.member (Char.toCode ' ') game.keysDown
    , delta = inSeconds delta
    , directions = horizontals ++ verticals
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


gameWidth : Float
gameWidth =
    600


gameHeight : Float
gameHeight =
    400


view : GameState -> Html Msg
view { windowDimensions, objects, player } =
    let
        ( w, h ) =
            windowDimensions
    in
    toHtml <|
        container w h middle <|
            collage (round gameWidth)
                (round gameHeight)
                (List.concat
                    [ [ rect gameWidth gameHeight
                            |> filled parquetFloorBrown
                      ]
                    , List.map
                        (\obj ->
                            rect (Tuple.first obj.size) (Tuple.second obj.size)
                                |> make obj obj.color
                        )
                        objects
                    , [ oval (Tuple.first player.size) (Tuple.second player.size)
                            |> make player white
                      ]
                    ]
                )


verticalLine : Float -> Path
verticalLine height =
    path [ ( 0, height ), ( 0, -height ) ]


parquetFloorBrown : Color
parquetFloorBrown =
    rgb 153 76 0


make : { a | position : ( Float, Float ) } -> Color -> Shape -> Form
make obj color shape =
    shape
        |> filled color
        |> move obj.position
