module Main exposing (..)

import Dict exposing (Dict)

import Browser

import HanoiView exposing (Discs, Model, Move(..), Msg, Tick, view)


main =
  Browser.sandbox { init = init, update = update, view = view }


init : Model
init = 
  { discs = Dict.fromList 
      [ (0, [2, 3, 4, 5, 6, 7, 8, 9, 10])
      , (1, [])
      , (2, [])
      ]
  , moves =
    [ Move 0 1
    , Move 0 2
    , Move 1 2
    ]
  }


update : Msg -> Model -> Model
update _ model =
  case model.moves of
    [] -> model
    (m::ms) -> {discs = updateDiscs m model.discs, moves = ms}

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
