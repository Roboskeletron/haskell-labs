module Point where

data Point = Point
  { x :: Double
  , y :: Double
  } deriving (Show, Eq)

moveX :: Double -> Point -> Point
moveX dx (Point x y) = Point (x + dx) y


moveY :: Double -> Point -> Point
moveY dy (Point x y) = Point x (y + dy)

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x*x + y*y)

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) =
  sqrt ((x1 - x2)^2 + (y1 - y2)^2)


toPolar :: Point -> (Double, Double)
toPolar (Point x y) = (distanceToOrigin (Point x y), atan2 y x)


main :: IO ()
main = do
  let p1 = Point 3 4
  let p2 = Point 0 0
  let p3 = moveX 5 p1          -- (8, 4)
  let p4 = moveY (-2) p1       -- (3, 2)

  putStrLn $ "Точка p1: " ++ show p1
  putStrLn $ "Точка p2: " ++ show p2
  putStrLn $ "Точка p3: " ++ show p3
  putStrLn $ "Точка p4: " ++ show p4
  putStrLn $ "Расстояние до начала: " ++ show (distanceToOrigin p1)
  putStrLn $ "Расстояние p1 до p2: " ++ show (distance p1 p2)
  putStrLn $ "Полярные координаты p1: " ++ show (toPolar p1)

  putStrLn $ "p1 совпадает с p3? " ++ show (p1 == p3)
  putStrLn $ "p1 отличается от p4? " ++ show (p1 /= p4)