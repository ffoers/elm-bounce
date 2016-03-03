module ArrowsR where

import Graphics.Element exposing  (..)
import Graphics.Collage exposing  (..)
import Color exposing (..)

import Window
import Keyboard
import Time


-- Model
fps = 30

type Action =   Resize (Int, Int) | Velocity (Float, Float)| Tick | NoOp

type alias Model =
  { x : Float,
    y : Float,
    velX : Float,
    velY : Float,
    width : Int,
    height : Int
  }

initialModel : Model
initialModel =
  { x= 0,
    y= 0,
    velX = 0,
    velY = 0,
    width = 200,
    height = 200
  }

-- Update

update : Action -> Model -> Model
update action model =
  case action of
    Tick ->
        bounceX model
        |>( \ model ->
        {model |
          x = model.x + model.velX,
          y = model.y + model.velY
        } )
    Velocity (x, y) ->
      { model |
        velX = model.velX + 0.5 * x,
        velY = model.velY + 0.5 * y
      }
    Resize(w, h) ->
      { model |
          width = w,
          height = h
      }
    NoOp ->
      model

bounceX : Model -> Model
bounceX model =
  let
    borderLeft = toFloat model.width / 2 * -1
    borderRight = toFloat model.width / 2
    borderBottom = toFloat model.height / 2 * -1
    borderTop = toFloat model.height / 2
  in
    if model.x < borderLeft then
      { model | velX  = if model.velX > 0 then model.velX else model.velX * -1 }
    else if model.x > borderRight then
      { model | velX  = if model.velX < 0 then model.velX else model.velX * -1 }
    else if model.y > borderTop then
      { model | velY  = if model.velY < 0 then model.velY else model.velY * -1 }
    else if model.y < borderBottom then
      { model | velY  = if model.velY > 0 then model.velY else model.velY * -1 }
    else
      model

-- Signals

model : Signal Model
model =
  Signal.foldp update initialModel actions

actions : Signal Action
actions =
  Signal.mergeMany
    [ resize,
      arrows,
      tick
    ]

tick : Signal Action
tick =
  Signal.map (\ _ -> Tick ) (Time.fps fps)

resize : Signal Action
resize =
  Signal.map (\ (w, h) -> Resize (w, h)) (Signal.sampleOn (Time.fps fps) Window.dimensions)

arrow : {x:Int, y:Int} -> Action
arrow {x, y} =
  let
    val = (toFloat x, toFloat y)
  in
    Velocity val

arrows : Signal Action
arrows =
  Signal.map arrow (Signal.sampleOn (Time.fps fps) Keyboard.arrows)

-- View

view : Model ->  Element
view model   =
  let
    w' = toFloat model.width
    h' = toFloat model.height
  in
    collage model.width model.height
    [ rect w' h' |> filled grey,
      rect 10 10 |> filled blue |> move (model.x, model.y),
      toForm (show model)
    ]


main =
   Signal.map view model
