module Main exposing (main)

import Array exposing (Array)
import Browser exposing (Document)
import Canvas
import Canvas.Settings
import Canvas.Settings.Line
import Color
import Html.Attributes exposing (id)
import List exposing (maximum)
import Logic.Component
import Logic.Entity exposing (EntityID)
import Logic.System
import Time


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { grid : Array Bool
    , position : Logic.Component.Set ( Int, Int )
    , flight : Logic.Component.Set Flight
    , nectar : Logic.Component.Set { has : Int, maximum : Int }
    , nectarExchange : List NectarExchangeRequest
    , goal : Logic.Component.Set Goal
    , carrying : Logic.Component.Set (Maybe EntityID)
    }


type alias NectarExchangeRequest =
    { from : EntityID
    , to : EntityID
    , quantity : Int
    }


carryingSpec : Logic.Component.Spec (Maybe EntityID) Model
carryingSpec =
    Logic.Component.Spec .carrying (\comp world -> { world | carrying = comp })


positionSpec : Logic.Component.Spec ( Int, Int ) Model
positionSpec =
    Logic.Component.Spec .position (\comp world -> { world | position = comp })


type Flight
    = Flight


flightSpec : Logic.Component.Spec Flight Model
flightSpec =
    Logic.Component.Spec .flight (\comp world -> { world | flight = comp })


nectarSpec : Logic.Component.Spec { has : Int, maximum : Int } Model
nectarSpec =
    Logic.Component.Spec .nectar (\comp world -> { world | nectar = comp })


type Goal
    = NoGoal
    | HasGoal (List ( Int, Int )) EntityID


goalSpec : Logic.Component.Spec Goal Model
goalSpec =
    Logic.Component.Spec .goal (\comp world -> { world | goal = comp })


init : () -> ( Model, Cmd Msg )
init () =
    ( Logic.Entity.create 0 initialWorld
        |> Logic.Entity.with ( positionSpec, ( width // 2, height // 2 ) )
        |> Logic.Entity.with ( flightSpec, Flight )
        |> Logic.Entity.with ( goalSpec, NoGoal )
        |> Logic.Entity.with ( nectarSpec, { has = 0, maximum = 10 } )
        |> Tuple.second
        |> Logic.Entity.create 1
        |> Logic.Entity.with ( positionSpec, ( 5, 5 ) )
        |> Logic.Entity.with ( nectarSpec, { has = 1000, maximum = 1000 } )
        |> Tuple.second
    , Cmd.none
    )


width =
    15


height =
    15


initialWorld : Model
initialWorld =
    { grid = Array.initialize (width * height) (\_ -> True)
    , position = Logic.Component.empty
    , flight = Logic.Component.empty
    , nectar = Logic.Component.empty
    , nectarExchange = []
    , goal = Logic.Component.empty
    , carrying = Logic.Component.empty
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 500 (\_ -> Tick)


type Msg
    = NoOp
    | Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick ->
            ( model
                |> requestNectarSystem positionSpec goalSpec nectarSpec
                |> nectarExchangeSystem
                |> (\m -> goalSystem m positionSpec goalSpec m)
                |> flightSystem positionSpec goalSpec flightSpec
            , Cmd.none
            )


nectarExchangeSystem : Model -> Model
nectarExchangeSystem model =
    nectarExchangeSystemHelper { model | nectarExchange = List.reverse model.nectarExchange }


nectarExchangeSystemHelper : Model -> Model
nectarExchangeSystemHelper model =
    case model.nectarExchange of
        [] ->
            model

        request :: rest ->
            case
                ( Logic.Component.get request.from model.nectar
                , Logic.Component.get request.to model.nectar
                )
            of
                ( Just nectarGiver, Just nectarReceiver ) ->
                    let
                        nectarToExhange : Int
                        nectarToExhange =
                            min request.quantity nectarGiver.has
                                |> Debug.log "to exchange"
                    in
                    nectarExchangeSystem
                        { model
                            | nectar =
                                model.nectar
                                    |> Logic.Component.set request.from { nectarGiver | has = nectarGiver.has - nectarToExhange }
                                    |> Logic.Component.set request.to { nectarReceiver | has = nectarReceiver.has + nectarToExhange }
                            , nectarExchange = rest
                        }

                _ ->
                    nectarExchangeSystem model


requestNectarSystem :
    Logic.Component.Spec ( Int, Int ) Model
    -> Logic.Component.Spec Goal Model
    -> Logic.Component.Spec { has : Int, maximum : Int } Model
    -> Logic.System.System Model
requestNectarSystem spec1 spec2 spec3 worldPrime =
    Logic.System.indexedFoldl3
        (\requesterId pos goal requesterNectar world ->
            case ( requesterNectar.has < requesterNectar.maximum, goal ) of
                ( True, HasGoal [] goalId ) ->
                    case Logic.Component.get2 goalId world.position world.nectar of
                        Nothing ->
                            world

                        Just ( goalPos, goalNectar ) ->
                            if pos == goalPos && goalNectar.has > 0 then
                                { world
                                    | nectarExchange =
                                        { from = goalId
                                        , to = requesterId
                                        , quantity =
                                            (requesterNectar.maximum - requesterNectar.has)
                                                |> Debug.log "request amount"
                                        }
                                            :: world.nectarExchange
                                }

                            else
                                world

                _ ->
                    world
        )
        (spec1.get worldPrime)
        (spec2.get worldPrime)
        (spec3.get worldPrime)
        worldPrime


goalSystem :
    Model
    -> Logic.Component.Spec ( Int, Int ) Model
    -> Logic.Component.Spec Goal Model
    -> Logic.System.System Model
goalSystem model =
    Logic.System.step2
        (\( pos, _ ) ( goal, setGoal ) ->
            case goal of
                HasGoal _ _ ->
                    identity

                NoGoal ->
                    Logic.System.indexedFoldl2
                        (\flowerId nectarPos nectar nearestGoal ->
                            if nectar.has > 0 then
                                case nearestGoal of
                                    Nothing ->
                                        Just ( nectarPos, flowerId )

                                    Just ( nearestPos, _ ) ->
                                        if distance pos nectarPos < distance pos nearestPos then
                                            Just ( nectarPos, flowerId )

                                        else
                                            nearestGoal

                            else
                                nearestGoal
                        )
                        model.position
                        model.nectar
                        Nothing
                        |> Maybe.map (\( nPos, id ) -> HasGoal (aStar pos nPos) id)
                        |> Maybe.withDefault NoGoal
                        |> setGoal
        )


aStar : ( Int, Int ) -> ( Int, Int ) -> List ( Int, Int )
aStar (( x, y ) as startPos) goalPos =
    aStarHelper startPos
        goalPos
        [ Tile
            { g = 0
            , h = distance startPos goalPos
            , parent = Nothing
            , x = x
            , y = y
            }
        ]
        []


type alias TileData =
    { g : Int
    , h : Int
    , parent : Maybe Tile
    , x : Int
    , y : Int
    }


type Tile
    = Tile TileData


aStarHelper : ( Int, Int ) -> ( Int, Int ) -> List Tile -> List Tile -> List ( Int, Int )
aStarHelper startPos (( goalX, goalY ) as goalPos) openList closedList =
    case openList of
        [] ->
            []

        (Tile next) :: rest ->
            let
                neighbors : List Tile
                neighbors =
                    List.filterMap
                        (\( x, y ) ->
                            if
                                (x < 0)
                                    || (x > width)
                                    || (y < 0)
                                    || (y > height)
                                    || List.any (\(Tile a) -> a.x == x && a.y == y) closedList
                                    || List.any (\(Tile a) -> a.x == x && a.y == y) openList
                            then
                                Nothing

                            else
                                Just
                                    (Tile
                                        { g = next.g + 1
                                        , h = distance ( next.x, next.y ) goalPos
                                        , parent = Just (Tile next)
                                        , x = x
                                        , y = y
                                        }
                                    )
                        )
                        [ ( next.x - 1, next.y )
                        , ( next.x + 1, next.y )
                        , ( next.x - 1, next.y - 1 )
                        , ( next.x + 1, next.y - 1 )
                        , ( next.x, next.y + 1 )
                        , ( next.x, next.y - 1 )
                        ]

                maybeGoalFound : List Tile
                maybeGoalFound =
                    List.filter (\(Tile a) -> a.x == goalX && a.y == goalY) neighbors
            in
            case maybeGoalFound of
                [ goalFound ] ->
                    rewind goalFound []

                _ ->
                    aStarHelper startPos
                        goalPos
                        (List.sortBy (\(Tile a) -> a.g + a.h) (neighbors ++ rest))
                        (Tile next :: closedList)


rewind : Tile -> List ( Int, Int ) -> List ( Int, Int )
rewind (Tile tile) path =
    case tile.parent of
        Nothing ->
            ( tile.x, tile.y ) :: path

        Just x ->
            rewind x (( tile.x, tile.y ) :: path)


distance : ( Int, Int ) -> ( Int, Int ) -> Int
distance ( x1, y1 ) ( x2, y2 ) =
    round (sqrt (toFloat ((x2 - x1) ^ 2) + toFloat ((y2 - y1) ^ 2)))


flightSystem :
    Logic.Component.Spec ( Int, Int ) Model
    -> Logic.Component.Spec Goal Model
    -> Logic.Component.Spec Flight Model
    -> Logic.System.System Model
flightSystem =
    Logic.System.step3
        (\( _, setPos ) ( goal, setGoal ) _ ->
            case goal of
                NoGoal ->
                    identity

                HasGoal [] _ ->
                    setGoal NoGoal

                HasGoal (goalPos :: rest) goalId ->
                    setGoal (HasGoal rest goalId) >> setPos goalPos
        )


view : Model -> Document Msg
view model =
    { title = "Hive"
    , body =
        [ Canvas.toHtml ( 800, 600 )
            []
            [ Canvas.shapes
                [ Canvas.Settings.fill Color.lightYellow ]
                [ Canvas.rect ( 0, 0 ) 800 600 ]
            , model.grid
                |> Array.toIndexedList
                |> List.map
                    (\( index, _ ) ->
                        let
                            x =
                                modBy width index

                            y =
                                toFloat (index // width)
                                    * 17
                                    + (if modBy 2 x == 0 then
                                        0

                                       else
                                        8.5
                                      )
                        in
                        drawHexagon ( toFloat x * 15 + 10, y + 10 )
                    )
                |> Canvas.shapes
                    [ Canvas.Settings.stroke Color.darkYellow
                    , Canvas.Settings.Line.lineWidth 2
                    ]
            , Logic.System.foldl2
                (\( x, y ) _ result ->
                    Canvas.rect
                        ( toFloat x * 15 + 11
                        , toFloat y
                            * 17
                            + (if modBy 2 x == 0 then
                                15

                               else
                                23.5
                              )
                        )
                        8
                        8
                        :: result
                )
                model.position
                model.nectar
                []
                |> Canvas.shapes
                    [ Canvas.Settings.stroke Color.darkGreen
                    , Canvas.Settings.Line.lineWidth 2
                    , Canvas.Settings.fill Color.green
                    ]
            , Logic.System.foldl2
                (\pos _ result ->
                    drawBee pos ++ result
                )
                model.position
                model.flight
                []
                |> Canvas.shapes
                    [ Canvas.Settings.stroke Color.black
                    , Canvas.Settings.Line.lineWidth 1
                    , Canvas.Settings.fill Color.yellow
                    ]
            ]
        ]
    }


drawBee : ( Int, Int ) -> List Canvas.Shape
drawBee ( x, y ) =
    [ Canvas.circle
        ( toFloat x * 15 + 15
        , toFloat y
            * 17
            + (if modBy 2 x == 0 then
                18

               else
                26.5
              )
        )
        4
    , Canvas.path
        ( toFloat x * 15 + 11
        , toFloat y
            * 17
            + (if modBy 2 x == 0 then
                18

               else
                26.5
              )
        )
        [ Canvas.lineTo
            ( toFloat x * 15 + 19
            , toFloat y
                * 17
                + (if modBy 2 x == 0 then
                    18

                   else
                    26.5
                  )
            )
        ]
    ]


drawHexagon : Canvas.Point -> Canvas.Shape
drawHexagon ( initialX, initialY ) =
    Canvas.path ( initialX, initialY )
        (List.foldl
            (\_ ( result, ( x, y, angle ) ) ->
                let
                    nextX =
                        x + 10 * cos (angle * pi / 180)

                    nextY =
                        y + 10 * sin (angle * pi / 180)
                in
                ( Canvas.lineTo ( nextX, nextY ) :: result
                , ( nextX, nextY, angle + 60 )
                )
            )
            ( [], ( initialX, initialY, 0 ) )
            (List.range 0 5)
            |> Tuple.first
            |> List.reverse
        )
