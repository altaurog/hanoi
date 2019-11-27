module HanoiView exposing (..)

import Dict exposing (Dict)

import Html exposing (Html, div, form, input)
import Html.Attributes
import Html.Events exposing (onClick, onInput)

import Svg exposing (..)
import Svg.Attributes exposing (..)

import Animation as A

class = Html.Attributes.class
classes = List.map class
classDiv c = div (classes c)
button bc action =
  Html.button [
    class "btn",
    class ("btn-" ++ bc),
    class "navbar-btn",
    typ "button",
    onClick action
  ]
role = Html.Attributes.attribute "role"
val = Html.Attributes.value
typ = Html.Attributes.type_
min = Html.Attributes.min
max = Html.Attributes.max
step = Html.Attributes.step

thickness = 3

type alias Discs = Dict Int (List Int)
type alias Model = 
  { num: Int
  , pegs: Discs
  , positions: List (Int, Int)
  , animState: List A.State
  , moves: List Move
  , speed: Float
  , play: Bool
  }

type Msg
  = NextMove
  | Animate A.Msg
  | PlayPause
  | Faster
  | Slower
  | Reset
  | SetNum Int
type Move = Move Int Int

view : Model -> Html Msg
view model =
  let pegs = List.map peg [1, 2, 3]
      ds = List.indexedMap disc model.animState
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
  in classDiv ["navbar", "navbar-fixed-top"]
    [ classDiv ["container-fluid"]
      [ classDiv ["navbar-header"]
          [ classDiv ["navbar-brand"] [Html.text "Tower of Hanoi"] ]
      , form (classes ["navbar-form", "navbar-left"])
          [ classDiv ["form-group"]
            [ button "primary" PlayPause [ Html.text playpause ]
            , input
                [ class "form-control"
                , typ "number"
                , val (String.fromInt num)
                , min "2"
                , max "12"
                , step "1"
                , onInput setnum
                ] []
            , button "default" Reset [ Html.text "Reset" ]
            , button "default" Slower [ Html.text "Slower" ]
            , button "default" Faster [ Html.text "Faster" ]
            ]
          ]
        ]
      ]

disc : Int -> A.State -> Svg msg
disc i astate =
  let dwidth = i + 2
  in rect (List.concat [A.render astate, discAttrs dwidth]) []

discAttrs : Int -> List (Attribute msg)
discAttrs dwidth =
  [ width <| percent (2 * dwidth)
  , height <| percent thickness
  , rx "5"
  , ry "5"
  , fill <| hsl (dwidth - 1)
  , stroke "#454545"
  , discId dwidth
  ]

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
    , y "50%"
    , width <| percent (2 * w)
    , height "50%"
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
