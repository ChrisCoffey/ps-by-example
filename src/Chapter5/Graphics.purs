module Chapter5.Graphics where

import Prelude
import Chapter5.Picture

import Data.Maybe (Maybe(..))
import Math

origin :: Point
origin = Point {x: 0.0, y: 0.0}

cAtTen :: Shape
cAtTen = Circle origin 10.0

scale :: Number -> Shape -> Shape
scale x (Circle c r) = Circle c (r * x)
scale x (Rectangle c l r) = (Rectangle c (l * x) (r * x))
scale a (Line x (Point {x: m, y: n})) = Line x (Point {x: a * m, y: a * n})
scale _ x =  x

getText :: Shape -> Maybe String
getText (Text _ cs) = Just cs
getText _           = Nothing

type Picture = Array Shape

showPicture :: forall t1. (Functor t1) => t1 Shape -> t1 String
showPicture = map showShape

area :: Shape -> Number
area (Circle _ r)       = r * r * pi
area (Rectangle _ l r)  = l * r
area _                  = 0.0


