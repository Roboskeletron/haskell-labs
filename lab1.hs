import Prelude hiding (sum)

ak :: Integer -> Double -> Double -> Double
ak n ap x = let
    k = fromIntegral n
    a = x ^ 2
    b = 2 * k * (2 * k - 1)
    in a / b * ap


ch x n ap s c e
    | n == c = s
    | otherwise = let
        nextA = ak c ap x
        sum = if abs nextA > e then s + nextA else s
        in ch x n nextA sum (c+1) e

sum x n = ch x n 1 1 1 0

sumGrE x n e
    | e > 1 = 0
    | e == 1 = 1
    | otherwise = ch x n 1 1 1 e

sumGrE10 x n e = sumGrE x n (e / 10)  

main = do
    xStr <- getLine
    nStr <- getLine
    eStr <- getLine

    let x = read xStr :: Double
        n = read nStr :: Integer
        e = read eStr :: Double
    
    print $ sum x n
    print $ sumGrE x n e
    print $ sumGrE10 x n e
    print $ cosh x