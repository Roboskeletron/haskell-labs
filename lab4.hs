import Data.List (unfoldr)

factorize :: Integer -> [Integer]
factorize 1 = [1]
factorize n = 1: unfoldr step n
  where
    step 1 = Nothing 
    step m = let 
        sqrtM = floor . sqrt $ fromIntegral m
        factors = filter (\x -> (m `mod` x) == 0) [2..sqrtM]
      in if null factors 
         then Just (m, 1)
         else let p = head factors
              in Just (p, m `div` p)

main = do
    print $ factorize 10
    print $ factorize 12
