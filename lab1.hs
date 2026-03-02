import Prelude hiding (sum)

ak :: Integer -> Double -> Double -> Double
ak n ap x =
  let k = fromIntegral n
      a = x ^ 2
      b = 2 * k * (2 * k - 1)
   in a / b * ap

ch x n ap s c
  | n == c = s
  | otherwise =
      let nextA = ak c ap x
       in ch x n nextA (s + nextA) (c + 1)

che x ap s c e
  | abs ap < e = s
  | otherwise =
      let nextA = ak c ap x
       in che x nextA (s + nextA) (c + 1) e

sum x n = ch x n 1 1 1

sumGrE x = che x 1 1 1

sumGrE10 x e = sumGrE x (e / 10)

main = do
  xStr <- getLine
  nStr <- getLine
  eStr <- getLine

  let x = read xStr :: Double
      n = read nStr :: Integer
      e = read eStr :: Double

  print $ sum x n
  print $ sumGrE x e
  print $ sumGrE10 x e
  print $ cosh x
