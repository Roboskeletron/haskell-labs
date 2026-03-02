removeAfterE :: Eq a => a -> [a] ->[a]
removeAfterE _ [] = []
removeAfterE _ [x] = [x]
removeAfterE e (x:y:xs)
    | x == e && y /= e = x : removeAfterE e xs
    | otherwise        = x : removeAfterE e (y:xs)

main = do
    let e = 1
    print $ removeAfterE e[1, 2, 3, 1, 4] 
    print $ removeAfterE e [1, 1, 2]       
    print $ removeAfterE e[2, 3, 1]       