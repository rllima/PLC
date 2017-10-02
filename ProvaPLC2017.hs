ehMatriz :: [[Int]] -> Bool
ehMatriz m = valorIgual (map length m)

valorIgual :: [Int] -> Bool
valorIgual (x:[]) = True
valorIgual (x:y:[]) = x == y
valorIgual(x:y:ys) = x == y && valorIgual ys

valorDiag :: [[Int]] -> [Int]
valorDiag [] = []
valorDiag [[]] = []
valorDiag ((x:xs):ys) = [x] ++ valorDiag (map (drop 1) ys)

troca :: [[Int]] -> Int -> Int -> [[Int]]
troca x y l = init (take x l) ++ [(take y l)!!(y-1)] ++ (take(x-y-1) (drop x l)) ++ [(take x l) !! (x-1)] ++ (drop y l)
