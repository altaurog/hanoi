module Main exposing (..)

import Dict exposing (Dict)

import Browser
import Platform
import Time

import HanoiView exposing (Discs, Model, Move(..), Msg(..), view)


main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }


nocmd : Model -> (Model, Cmd Msg)
nocmd model = (model, Cmd.none)

init : () -> (Model, Cmd Msg)
init _ = nocmd <| reset 9

reset : Int -> Model
reset n =
    { num = n
    , discs = Dict.fromList 
        [ (0, List.range 2 (n + 1))
        , (1, [])
        , (2, [])
        ]
    , moves = solution n 0 1 2
    , speed = 500.0
    , play = False
    }

subscriptions : Model -> Sub Msg
subscriptions {play, speed} =
  if play then Time.every speed <| always Tick else Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = nocmd <|
  case msg of
    SetNum n -> {model | num = n}
    PlayPause -> {model | play = not model.play}
    Faster -> {model | speed = limit <| model.speed * 0.8}
    Slower -> {model | speed = limit <| model.speed * 1.25 }
    Reset -> reset model.num
    Tick ->
      case model.moves of
        [] -> model
        (m::ms) -> {model | discs = updateDiscs m model.discs, moves = ms}

limit : Float -> Float
limit = clamp 250 1500

updateDiscs : Move -> Discs -> Discs
updateDiscs (Move f t) discs =
  case (Dict.get f discs, Dict.get t discs) of
    (Just (d::fds), Just tds) -> move f fds t (d::tds) discs
    (_, _) -> discs


move : Int -> List Int -> Int -> List Int -> Discs -> Discs
move f fds t tds =
  if tds == List.sort tds then
      Dict.union (Dict.fromList [(f, fds), (t, tds)])
  else
      identity


solution : Int -> Int -> Int -> Int -> List Move
solution n a b c =
  if n == 0 then []
  else
    let
      toC = solution (n - 1) a c b
      toB = solution (n - 1) c b a
    in toC ++ [Move a b] ++ toB
