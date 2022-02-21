module Main exposing (main)

import Array exposing (Array)
import Browser exposing (Document)
import Canvas
import Canvas.Settings
import Canvas.Settings.Line
import Color
import Logic.Component
import Logic.Entity
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
    , food : Logic.Component.Set Food
    , goal : Logic.Component.Set (Maybe ( Int, Int ))
    }


positionSpec : Logic.Component.Spec ( Int, Int ) Model
positionSpec =
    Logic.Component.Spec .position (\comp world -> { world | position = comp })


type Flight
    = Flight


flightSpec : Logic.Component.Spec Flight Model
flightSpec =
    Logic.Component.Spec .flight (\comp world -> { world | flight = comp })


type Food
    = Food


foodSpec : Logic.Component.Spec Food Model
foodSpec =
    Logic.Component.Spec .food (\comp world -> { world | food = comp })


goalSpec : Logic.Component.Spec (Maybe ( Int, Int )) Model
goalSpec =
    Logic.Component.Spec .goal (\comp world -> { world | goal = comp })


init : () -> ( Model, Cmd Msg )
init () =
    ( Logic.Entity.create 0 initialWorld
        |> Logic.Entity.with ( positionSpec, ( 0, 0 ) )
        |> Logic.Entity.with ( flightSpec, Flight )
        |> Logic.Entity.with ( goalSpec, Nothing )
        |> Tuple.second
        |> Logic.Entity.create 1
        |> Logic.Entity.with ( positionSpec, ( 5, 5 ) )
        |> Logic.Entity.with ( foodSpec, Food )
        |> Tuple.second
    , Cmd.none
    )


initialWorld : Model
initialWorld =
    { grid = Array.initialize 1768 (\_ -> True)
    , position = Logic.Component.empty
    , flight = Logic.Component.empty
    , food = Logic.Component.empty
    , goal = Logic.Component.empty
    }


subscriptions : Model -> Sub Msg
subscriptions model =
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
                |> goalSystem model positionSpec goalSpec
                |> flightSystem positionSpec goalSpec flightSpec
            , Cmd.none
            )


goalSystem : Model -> Logic.Component.Spec ( Int, Int ) Model -> Logic.Component.Spec (Maybe ( Int, Int )) Model -> Logic.System.System Model
goalSystem model =
    Logic.System.step2
        (\( pos, _ ) ( goal, setGoal ) ->
            case goal of
                Just goalPos ->
                    if pos == goalPos then
                        setGoal Nothing

                    else
                        identity

                Nothing ->
                    Logic.System.foldl2
                        (\foodPos _ nearestGoal ->
                            case nearestGoal of
                                Nothing ->
                                    Just foodPos

                                Just nearestPos ->
                                    if distance pos foodPos < distance pos nearestPos then
                                        Just foodPos

                                    else
                                        nearestGoal
                        )
                        model.position
                        model.food
                        Nothing
                        |> setGoal
        )


distance : ( Int, Int ) -> ( Int, Int ) -> Int
distance ( x1, y1 ) ( x2, y2 ) =
    round (sqrt (toFloat ((x2 - x1) ^ 2) + toFloat ((y2 - y1) ^ 2)))


flightSystem : Logic.Component.Spec ( Int, Int ) Model -> Logic.Component.Spec (Maybe ( Int, Int )) Model -> Logic.Component.Spec Flight Model -> Logic.System.System Model
flightSystem =
    Logic.System.step3
        (\( ( x, y ), setPos ) ( goal, _ ) _ ->
            case goal of
                Nothing ->
                    identity

                Just ( goalX, goalY ) ->
                    if x == goalX then
                        setPos
                            ( x
                            , if y < goalY then
                                y + 1

                              else
                                y - 1
                            )

                    else
                        setPos
                            ( if x < goalX then
                                x + 1

                              else
                                x - 1
                            , y
                            )
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
                            width =
                                52

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
                model.food
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
