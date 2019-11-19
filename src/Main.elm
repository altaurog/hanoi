module Main exposing (..)

import Dict exposing (Dict)

import Browser
import Html exposing (Html, button, div)
import Html.Events exposing (onClick)

import Svg exposing (..)
import Svg.Attributes exposing (..)


-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model = Dict Int (List Int)


init : Model
init = Dict.fromList 
  [ (0, [2, 3, 4, 5, 6, 7, 8, 9, 10])
  , (1, [])
  , (2, [])
  ]



-- UPDATE


type Msg
  = Move Int Int


update : Msg -> Model -> Model
update (Move f t) model =
  case (Dict.get f model, Dict.get t model) of
    (Just (d::fds), Just tds) -> move f fds t (d::tds) model
    (_, _) -> model


move : Int -> List Int -> Int -> List Int -> Model -> Model
move f fds t tds =
  if tds == List.sort tds then
      Dict.union (Dict.fromList [(f, fds), (t, tds)])
  else
      identity


-- VIEW


view : Model -> Html Msg
view model =
  let pegs = List.map peg [1, 2, 3]
      ds = List.concat <| Dict.values <| Dict.map discs model
  in div []
    [ div []
      [ Html.button [onClick (Move 0 1)] [Html.text "move"]
      ]
    , div []
      [ svg
        [ Svg.Attributes.style "width:100%"
        , height "500"
        ]
        (pegs ++ ds)
      ]
    ]

discs : Int -> List Int -> List (Svg msg)
discs n = List.reverse >> List.indexedMap (disc (n + 1))


disc : Int -> Int -> Int -> Svg msg
disc n h w =
  let mid = 25 * n
      t = 3
  in rect
    [ x <| percent (mid - w)
    , y <| percent (100 - t * (h + 1))
    , width <| percent (2 * w)
    , height <| percent t
    , rx "5"
    , ry "5"
    , fill <| hsl (w - 1)
    , stroke "#454545"
    , discId w
    ]
    []

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
