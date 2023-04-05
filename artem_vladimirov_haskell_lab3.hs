import Data.Monoid

data Point = Point {x :: Double, y :: Double} deriving (Eq, Show)

data Font = Consolas | LucidaConsole | SourceCodePro deriving (Eq, Read, Show)

fontArea :: Font -> Double
fontArea Consolas = 1.0
fontArea LucidaConsole = 1.2
fontArea SourceCodePro = 1.5

data Shape
  = Circle Point Double
  | Rectangle Point Point
  | Triangle Point Point Point
  | Label Point Font String
  deriving (Eq, Show)

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
  move :: (Double, Double) -> a -> a

instance Move Shape where
  move (dx, dy) (Circle (Point x y) r) = Circle (Point (x + dx) (y + dy)) r
  move (dx, dy) (Rectangle (Point x1 y1) (Point x2 y2)) =
    Rectangle (Point (x1 + dx) (y1 + dy)) (Point (x2 + dx) (y2 + dy))
  move (dx, dy) (Triangle (Point x1 y1) (Point x2 y2) (Point x3 y3)) =
    Triangle (Point (x1 + dx) (y1 + dy)) (Point (x2 + dx) (y2 + dy)) (Point (x3 + dx) (y3 + dy))
  move (dx, dy) (Label (Point x y) font text) = Label (Point (x + dx) (y + dy)) font text

-- Monoid і Foldable
data Plane shape = Leaf (Maybe shape) | Node (Plane shape) (Plane shape) deriving (Show)

instance Foldable Plane where
  foldr f acc (Leaf Nothing) = acc
  foldr f acc (Leaf (Just shape)) = f shape acc
  foldr f acc (Node left right) = foldr f (foldr f acc right) left

instance Semigroup (Plane shape) where
  left <> right = Node left right

instance Monoid (Plane shape) where
  mempty = Leaf Nothing

main :: IO ()
main = do
  let circle = Just (Circle (Point 0 0) 1)
      rectangle = Just (Rectangle (Point 1 1) (Point 2 2))
      triangle = Just (Triangle (Point 2 2) (Point 3 3) (Point 4 1))
      label = Just (Label (Point 3 3) LucidaConsole "Hello World!")
      plane1 = Node (Leaf circle) (Leaf rectangle)
      plane2 = Node (Leaf triangle) (Leaf label)
      plane3 = Node (Leaf triangle) (Leaf label)
      concatenatedPlane = plane1 <> plane2 <> plane3
      -- 1.1 area of shapes works
      areas = foldMap (\s -> "Area of shape " ++ show s ++ ": " ++ show (area s) ++ "\n") concatenatedPlane
      -- 1.2 get figures of selected type
      triangles = foldMap (\s -> "Triangle: " ++ show s ++ "\n") $ foldr (\x acc -> if figureType x == TriangleType then x : acc else acc) [] concatenatedPlane
      -- 1.3 bounding boxes of figures
      boundingBoxes = foldMap (\s -> "Bounding box of shape " ++ show s ++ ": " ++ show (boundingBox s) ++ "\n") concatenatedPlane
      -- 1.4 find all figures that belong to certain square
      square = Rectangle (Point 2 1) (Point 4 3)
      figuresInSquare = foldMap (\s -> "Belongs to " ++ show square ++ ": " ++ show s ++ "\n") $ foldr (\x acc -> if x < square then x : acc else acc) [] concatenatedPlane
      -- 1.5 move shapes by vector
      shiftedFigures = foldMap (\s -> "Shifted shape " ++ show s ++ ": " ++ show (move (1.0, 1.0)  s) ++ "\n") concatenatedPlane
  putStrLn areas
  putStrLn triangles
  putStrLn boundingBoxes
  putStrLn figuresInSquare