module Main exposing (..)

import Dict exposing (Dict)

import Browser
import Platform
import Time

import Animation as A

import HanoiView exposing (Discs, Model, Move(..), Msg(..), view, thickness)


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
  let
    pegs = Dict.fromList 
      [ (0, List.range 2 (n + 1))
      , (1, [])
      , (2, [])
      ]
    pos = positions pegs
  in
    { num = n
    , pegs = pegs
    , positions = pos
    , animState = List.map A.style <| List.map posProps pos
    , moves = solution n 0 1 2
    , speed = 500.0
    , play = False
    }

subscriptions : Model -> Sub Msg
subscriptions {moves, play, speed, animState} =
  let
    anim = A.subscription Animate animState
    tick = Time.every speed <| always NextMove
  in case moves of
    [] -> anim
    _ -> if play then Sub.batch [tick, anim] else anim

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = nocmd <|
  case msg of
    SetNum n -> {model | num = n}
    PlayPause -> {model | play = not model.play}
    Faster -> {model | speed = limit <| model.speed * 0.8}
    Slower -> {model | speed = limit <| model.speed * 1.25 }
    Reset -> reset model.num
    Animate animMsg ->
      {model | animState = List.map (A.update animMsg) model.animState}
    NextMove ->
      case model.moves of
        [] -> model
        (m::ms) ->
          let
            pegs = updateDiscs m model.pegs
            pos = positions pegs
            diff = List.map2 posDiff model.positions pos
          in { model
             | pegs = pegs
             , positions = pos
             , animState = List.map2 (updatePos model.speed) model.animState diff
             , moves = ms
             }

limit : Float -> Float
limit = clamp 250 1500

updateDiscs : Move -> Discs -> Discs
updateDiscs (Move f t) pegs =
  case (Dict.get f pegs, Dict.get t pegs) of
    (Just (d::fds), Just tds) -> move f fds t (d::tds) pegs
    (_, _) -> pegs


move : Int -> List Int -> Int -> List Int -> Discs -> Discs
move f fds t tds =
  if tds == List.sort tds then
      Dict.union (Dict.fromList [(f, fds), (t, tds)])
  else
      identity

positions : Discs -> List (Int, Int)
positions =
  Dict.map pegPositions
  >> Dict.values
  >> List.concat
  >> List.sort
  >> List.map Tuple.second

pegPositions : Int -> List Int -> List (Int, (Int, Int))
pegPositions n = List.reverse >> List.indexedMap (discPosition (n + 1))

discPosition : Int -> Int -> Int -> (Int, (Int, Int))
discPosition npeg h dwidth =
  let
    mid = 25 * npeg
    x = mid - dwidth
    y = 100 - thickness * (h + 1)
  in (dwidth, (x, y))

posDiff : (Int, Int) -> (Int, Int) -> Maybe ((Int, Int), (Int, Int))
posDiff old new =
  if old == new then Nothing else Just (old, new)

updatePos : Float -> A.State -> Maybe ((Int, Int), (Int, Int)) -> A.State
updatePos speed animState diff =
  case diff of
    Nothing -> animState
    Just pd -> A.queue (animSegments speed pd) animState

animSegments : Float -> ((Int, Int), (Int, Int)) -> List A.Step
animSegments speed ((ox, oy), (nx, ny)) =
  let
    ds = fracs [abs nx - ox, oy - 45, ny - 45]
    eas = List.map (easing speed) ds
    segments = [(ox, 45), (nx, 45), (nx, ny)]
  in List.map2 segment eas segments

fracs : List Int -> List Float
fracs is =
  let
    fs = List.map (abs >> toFloat) is
    total = List.sum fs
  in List.map (\x -> x / total) fs

easing : Float -> Float -> A.Interpolation
easing speed d = A.easing {duration = speed * 0.8 * d, ease = identity}

segment : A.Interpolation -> (Int, Int) -> A.Step
segment e p = A.toWith e (posProps p)

posProps : (Int, Int) -> List A.Property
posProps (x, y) =
  [ A.attr "x" (toFloat x) "%"
  , A.attr "y" (toFloat y) "%"
  ]

solution : Int -> Int -> Int -> Int -> List Move
solution n a b c =
  if n == 0 then []
  else
    let
      toC = solution (n - 1) a c b
      toB = solution (n - 1) c b a
    in toC ++ [Move a b] ++ toB
