module Lib
  ( Point(..),
    Vector(..),
    Font(..),
    Shape (..),
    Plane(..),
    FigureType(..),
    createShape,
    readShape,
    updateShape,
    deleteShape,
    areaShape,
    typeShape,
    boxShape,
    containedShape,
    moveShape
  )
where

import Data.Maybe

data Point = Point {x :: Double, y :: Double} deriving (Eq, Read, Show)

newtype Vector = Vector Point deriving (Show)

data Font = Consolas | LucidaConsole | SourceCodePro deriving (Eq, Show)

instance Read Font where
  readsPrec _ input = case input of
    "Consolas" -> [(Consolas, "")]
    "LucidaConsola" -> [(LucidaConsole, "")]
    "SourceCodePro" -> [(SourceCodePro, "")]
    _ -> []


fontArea :: Font -> Double
fontArea Consolas = 1.0
fontArea LucidaConsole = 1.2
fontArea SourceCodePro = 1.5

data Shape
  = Circle Point Double
  | Rectangle Point Point
  | Triangle Point Point Point
  | Label Point Font String
  deriving (Eq, Read, Show)

-- 1.1 обчислення площi фiгури
class Area a where
  area :: a -> Double

instance Area Shape where
  area (Circle _ r) = pi * r ^ 2
  area (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * abs (y2 - y1)
  area (Triangle (Point x1 y1) (Point x2 y2) (Point x3 y3)) = 0.5 * abs ((x1 - x3) * (y2 - y1) - (x1 - x2) * (y3 - y1))
  area (Label (Point x y) font text) = fromIntegral (length text) * fontArea font

-- 1.2 отримання списку фігур вказаного типу
data FigureType = CircleType | RectangleType | TriangleType | LabelType deriving (Eq, Show)

instance Read FigureType where
  readsPrec _ input = case input of
    "CircleType" -> [(CircleType, "")]
    "RectangleType" -> [(RectangleType, "")]
    "TriangleType" -> [(TriangleType, "")]
    "LabelType" -> [(LabelType, "")]
    _ -> []

class Figure a where
  figureType :: a -> FigureType

instance Figure Shape where
  figureType (Circle {}) = CircleType
  figureType (Rectangle {}) = RectangleType
  figureType (Triangle {}) = TriangleType
  figureType (Label {}) = LabelType

-- 1.3 отримання прямокутника, що охоплює вказану фiгуру
class BoundingBox a where
  boundingBox :: a -> Shape

instance BoundingBox Shape where
  boundingBox (Circle (Point x y) r) = Rectangle (Point (x - r) (y - r)) (Point (x + r) (y + r))
  boundingBox (Rectangle p1 p2) = Rectangle (Point (min (x p1) (x p2)) (min (y p1) (y p2))) (Point (max (x p1) (x p2)) (max (y p1) (y p2)))
  boundingBox (Triangle p1 p2 p3) = Rectangle (Point (minimum [x p1, x p2, x p3]) (minimum [y p1, y p2, y p3])) (Point (maximum [x p1, x p2, x p3]) (maximum [y p1, y p2, y p3]))
  boundingBox (Label (Point x y) font text) =
    let w = fromIntegral (length text) * 1.0
        h = fontArea font
     in Rectangle (Point x y) (Point (x + w) (y + h))

-- 1.4 пошук фiгур, якi знаходяться у вказаному квадратi на площинi
containedIn :: Shape -> Shape -> Bool
containedIn (Rectangle (Point x1 y1) (Point x2 y2)) (Rectangle (Point x3 y3) (Point x4 y4)) =
  min x1 x2 >= min x3 x4 && max x1 x2 <= max x3 x4 && min y1 y2 >= min y3 y4 && max y1 y2 <= max y3 y4

instance Ord Shape where
  compare s1 s2 =
    let bb1 = boundingBox s1
        bb2 = boundingBox s2
     in if bb1 `containedIn` bb2
          then LT
          else GT

-- 1.5 перемiщення фiгури на вказаний вектор
class Move a where
  move :: Vector -> a -> a

instance Move Shape where
  move (Vector (Point dx dy)) (Circle (Point x y) r) = Circle (Point (x + dx) (y + dy)) r
  move (Vector (Point dx dy)) (Rectangle (Point x1 y1) (Point x2 y2)) =
    Rectangle (Point (x1 + dx) (y1 + dy)) (Point (x2 + dx) (y2 + dy))
  move (Vector (Point dx dy)) (Triangle (Point x1 y1) (Point x2 y2) (Point x3 y3)) =
    Triangle (Point (x1 + dx) (y1 + dy)) (Point (x2 + dx) (y2 + dy)) (Point (x3 + dx) (y3 + dy))
  move (Vector (Point dx dy)) (Label (Point x y) font text) = Label (Point (x + dx) (y + dy)) font text

type Plane = [Maybe Shape]

--CRUD
-- Create
createShape :: Maybe Shape -> Plane -> Plane
createShape Nothing p = p
createShape (Just s) p = Just s : p

-- Read
readShape :: Int -> Plane -> Maybe Shape
readShape i p
  | i < 0 || i >= length p = Nothing
  | otherwise = p !! i

-- Update
updateShape :: Int -> Maybe Shape -> Plane -> Plane
updateShape i Nothing p = p
updateShape i (Just s) p
  | i < 0 || i >= length p = p
  | otherwise = take i p ++ [Just s] ++ drop (i + 1) p

-- Delete
deleteShape :: Int -> Plane -> Plane
deleteShape i p
  | i < 0 || i >= length p = p
  | otherwise = take i p ++ drop (i + 1) p

-- functions from lab 3
areaShape :: Int -> Plane -> Maybe Double
areaShape i p 
  | i < 0 || i >= length p || isNothing  (p !! i) = Nothing
  | otherwise = area <$> p !! i

typeShape :: FigureType -> Plane -> Plane
typeShape f = filter (maybe False (\ s -> figureType s == f))

boxShape :: Int -> Plane -> Maybe Shape
boxShape i p 
  | i < 0 || i >= length p || isNothing  (p !! i) = Nothing
  | otherwise = boundingBox <$> p !! i

containedShape :: Maybe Shape -> Plane -> Plane
containedShape Nothing p = []
containedShape (Just r) p = filter (maybe False (<r)) p

moveShape :: Int -> Maybe Vector -> Plane -> Plane
moveShape _ Nothing p = p
moveShape i (Just v) p = foldr (\x acc -> if length acc == i - 1 then (move v <$> x) : acc else x : acc) [] p








