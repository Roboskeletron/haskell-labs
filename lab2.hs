rowSums :: [[Double]] -> [Double]
rowSums = map sum

main = do
    let matrix = [[1.5, 2.0, 3.5],
                  [4.0, 5.0, 6.0],
                  [7.2, 8.1, 9.0]]

    let result = rowSums matrix
    print result