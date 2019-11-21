module HanoiView exposing (..)

import Dict exposing (Dict)

import Html exposing (Html, button, div, input)
import Html.Attributes
import Html.Events exposing (onClick, onInput)

import Svg exposing (..)
import Svg.Attributes exposing (..)

val = Html.Attributes.value
typ = Html.Attributes.type_
min = Html.Attributes.min
max = Html.Attributes.max
step = Html.Attributes.step

thickness = 3

type alias Discs = Dict Int (List Int)
type alias Model = 
  { num: Int
  , discs: Discs
  , pos: List (Int, Int, Int)
  , moves: List Move
  , speed: Float
  , play: Bool
  }

type Msg = Tick | PlayPause | Faster | Slower | Reset | SetNum Int
type Move = Move Int Int

view : Model -> Html Msg
view model =
  let pegs = List.map peg [1, 2, 3]
      ds = List.map disc model.pos
  in div []
    [ controls model
    , svg
      [Svg.Attributes.style "width:100%" , height "500"]
      (pegs ++ ds)
    ]

controls : Model -> Html Msg
controls {play, num} =
  let
    playpause = if play then "Pause" else "Play"
    setnum = String.toInt >> Maybe.withDefault num >> SetNum
  in div []
    [ input
      [ typ "number"
      , val (String.fromInt num)
      , min "2"
      , max "12"
      , step "1"
      , onInput setnum
      ] []
    , button [ onClick PlayPause ] [ Html.text playpause ]
    , button [ onClick Reset ] [ Html.text "Reset" ]
    , button [ onClick Slower ] [ Html.text "Slower" ]
    , button [ onClick Faster ] [ Html.text "Faster" ]
    ]

disc : (Int, Int, Int) -> Svg msg
disc (dwidth, x_, y_) =
  rect
    [ x <| percent x_
    , y <| percent y_
    , width <| percent (2 * dwidth)
    , height <| percent thickness
    , rx "5"
    , ry "5"
    , fill <| hsl (dwidth - 1)
    , stroke "#454545"
    , discId dwidth
    ] []

discId : Int -> Svg.Attribute msg
discId =
  String.fromInt
  >> String.padLeft 2 '0'
  >> ((++) "disc-")
  >> id

peg : Int -> Svg msg
peg n =
  let mid = 25 * n
      w = 1
  in rect 
    [ x <| percent (mid - w)
    , y "10%"
    , width <| percent (2 * w)
    , height "90%"
    , rx "2"
    , ry "2"
    , fill "#eeeeee"
    , stroke "#454545"
    ]
    []

percent : Int -> String
percent p =  (String.fromInt p) ++ "%"

hsl : Int -> String
hsl n =
  let h = 36 * n
  in "hsl(" ++ (String.fromInt h) ++ ", 100%, 50%)"
