module Main exposing (..)

import Dict exposing (Dict)

import Browser

import HanoiView exposing (Model, Msg(..), view)


main =
  Browser.sandbox { init = init, update = update, view = view }


init : Model
init = Dict.fromList 
  [ (0, [2, 3, 4, 5, 6, 7, 8, 9, 10])
  , (1, [])
  , (2, [])
  ]


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
