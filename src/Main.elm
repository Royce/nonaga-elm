module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick)
import Svg exposing (Svg, svg, circle)
import Svg.Attributes exposing (..)
import Color exposing (..)
import Color.Convert exposing (colorToHex)


main =
    App.beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Point =
    { x : Int, y : Int }


type Msg
    = MarbleSelected Point
    | MarbleMoved Point
    | RingSelected Point
    | RingMoved Point


type Step
    = Msg
    | TurnBegan


type alias Model =
    { rings : List Point
    , one : List Point
    , two : List Point
    , current : Int
    , step : Step
    , selected : Maybe Point
    }


model : Model
model =
    { rings =
        [ {- -} (Point 1 0)
        , (Point 2 0)
        , (Point 3 0)
        , {- -} (Point 0 1)
        , (Point 1 1)
        , (Point 2 1)
        , (Point 3 1)
        , (Point 0 2)
        , (Point 1 2)
        , (Point 2 2)
        , (Point 3 2)
        , (Point 4 2)
        , {- -} (Point 0 3)
        , (Point 1 3)
        , (Point 2 3)
        , (Point 3 3)
        , {- -} (Point 1 4)
        , (Point 2 4)
        , (Point 3 4)
        ]
    , one = [ (Point 1 0), (Point 4 2), (Point 1 4) ]
    , two = [ (Point 3 0), (Point 0 2), (Point 3 4) ]
    , current = 1
    , step = TurnBegan
    , selected = Nothing
    }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    model



-- case msg of
--   Noop ->
--     model
-- var steps = {
--   ne: function (coord) { return [coord[0]+(coord[1]%2 ? 1 : 0), coord[1]-1]; },
--   e: function (coord) { return [coord[0]+1, coord[1]]; },
--   se: function (coord) { return [coord[0]+(coord[1]%2 ? 1 : 0), coord[1]+1]; },
--   sw: function (coord) { return [coord[0]-(coord[1]%2 ? 0 : 1), coord[1]+1]; },
--   w: function (coord) { return [coord[0]-1, coord[1]]; },
--   nw: function (coord) { return [coord[0]-(coord[1]%2 ? 0 : 1), coord[1]-1]; },
-- }
-- VIEW


lighter : Color -> Color
lighter c =
    if c == Color.red then
        Color.lightRed
    else if c == Color.blue then
        Color.lightBlue
    else
        Color.lightGrey


view : Model -> Html Msg
view model =
    div []
        [ svg [ width "400px", height "400px" ]
            (List.map (ring { selectable = False }) model.rings
                ++ List.map (marble Color.red True) model.one
                ++ List.map (marble Color.blue False) model.two
            )
        ]


ring : { selectable : Bool } -> Point -> Svg Msg
ring { selectable } p =
    let
        p =
            translate p
    in
        circle
            ([ cx (toString p.x)
             , cy (toString p.y)
             , r "18"
             , fill "transparent"
             , stroke "#333"
             , strokeWidth "5px"
               --         stroke: props.highlight ? "#bbb" :
               --           (props.color ? lighterColor[props.color] : "#333"),
               --         'stroke-width': props.feint ? "1px" : "5px",
               --         'stroke-dasharray': (props.highlight || props.feint) ? "5,5" : "0"
             ]
                ++ if selectable then
                    [ onClick (RingSelected p)
                    , cursor "pointer"
                    ]
                   else
                    []
            )
            []


marble : Color -> Bool -> Point -> Svg Msg
marble color selectable p =
    let
        p =
            translate p
    in
        circle
            ([ cx (toString p.x)
             , cy (toString p.y)
             , r "14"
             , fill (colorToHex color)
             ]
                ++ if selectable then
                    [ onClick (MarbleSelected p)
                    , cursor "pointer"
                    ]
                   else
                    []
            )
            []


translate : Point -> Point
translate p =
    let
        xInterval =
            44

        yInterval =
            round (xInterval / 2 * sqrt (3))

        offset =
            xInterval // 2
    in
        (Point
            (80 + xInterval * p.x
                + (if (isOdd p.y) then
                    offset
                   else
                    0
                  )
            )
            (80 + yInterval * p.y)
        )


isOdd x =
    x % 2 == 1
