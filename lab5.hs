ak x ap k = a / b \* ap where  
a = x ^ 2  
b = 2 \* k  (2  k - 1)

ch x = scanl (ak x) 1 \[1..\]  
chn x n = sum $ take n (ch x)  
che x e = sum $ takeWhile (>e) (ch x)  
che10 x e = che x (e / 10)

main = do  
print $ chn 2 1000  
print $ che 2 0.001  
print $ che10 2 0.001  
print $ cosh 2