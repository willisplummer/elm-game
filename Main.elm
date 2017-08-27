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
    }


type alias PositionObject =
    { color : Color, kind : Object, position : ( Float, Float ), shape : Shape }


initialObjects : List PositionObject
initialObjects =
    [ { color = green, kind = Bed, position = ( gameWidth / 2, gameHeight / 2 ), shape = rect 80 160 } ]


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



-- UPDATE


updateGame : KeyInput -> GameState -> GameState
updateGame { delta, horizontalDirection, verticalDirection } ({ player } as game) =
    let
        isColliding =
            checkCollisions game
    in
    if isColliding then
        game
    else
        { game
            | player = updatePlayer delta horizontalDirection verticalDirection player
        }


checkCollisions : GameState -> Bool
checkCollisions { player, objects } =
    List.member True (List.map (\x -> x.position == player.position) objects)


updatePlayer : Time -> HorizontalDirection -> VerticalDirection -> Player -> Player
updatePlayer t hdir vdir ({ position, orientation } as player) =
    let
        rateOfMovement =
            200

        oldHPos =
            Tuple.first position

        newHPos =
            case hdir of
                HLeft ->
                    oldHPos - rateOfMovement * t

                HRight ->
                    oldHPos + rateOfMovement * t

                HorizontalNeutral ->
                    oldHPos

        oldVPos =
            Tuple.second position

        newVPos =
            case vdir of
                VDown ->
                    oldVPos - rateOfMovement * t

                VUp ->
                    oldVPos + rateOfMovement * t

                VerticalNeutral ->
                    oldVPos
    in
    { player
        | position = ( newHPos, newVPos )
    }


type HorizontalDirection
    = HLeft
    | HRight
    | HorizontalNeutral


type VerticalDirection
    = VUp
    | VDown
    | VerticalNeutral


type alias KeyInput =
    { space : Bool, horizontalDirection : HorizontalDirection, verticalDirection : VerticalDirection, delta : Float }


getInput : GameState -> Float -> KeyInput
getInput game delta =
    { space = Set.member (Char.toCode ' ') game.keysDown
    , delta = inSeconds delta
    , horizontalDirection =
        if Set.member 37 game.keysDown && Set.member 39 game.keysDown then
            HorizontalNeutral
        else if Set.member 37 game.keysDown then
            HLeft
        else if Set.member 39 game.keysDown then
            HRight
        else
            HorizontalNeutral
    , verticalDirection =
        if Set.member 38 game.keysDown && Set.member 40 game.keysDown then
            VerticalNeutral
        else if Set.member 38 game.keysDown then
            VUp
        else if Set.member 40 game.keysDown then
            VDown
        else
            VerticalNeutral
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
                            obj.shape
                                |> make obj obj.color
                        )
                        objects
                    , [ oval 15 15
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
