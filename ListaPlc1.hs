mdc :: Int -> Int -> Int
mdc a b
 |a == 0  = b
 |a > 0 = mdc(mod b a) a

isPrime :: Int->Bool
isPrime x
 |x == 2 = True
 |otherwise = null [y | y<-[2..floor (sqrt (fromIntegral x))], mod x y == 0]

type Ponto = (Double, Double, Double)
distP :: Ponto -> Ponto -> Double
distP (a,b,c) (d,e,f) = sqrt ((a-d)^2 + (b-e)^2 + (c - f)^2)

somaX = sum[x^2 | x <- [1..100]]

grid :: Int -> Int -> [(Int,Int)]
grid x y = [(a,b) | a <- [0..x] , b<-[0..y] ]

square :: Int -> [(Int,Int)]
square x = [(a,b) | (a,b) <- (grid x x) , a /= b]

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
 |(x<=y) = x : (merge xs (y:ys))
 |otherwise = y: (merge ys (x:xs))

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort (take(div (length xs)  2) xs)) (mergeSort (drop(div (length xs) 2 ) xs))

aplicaFunc :: [Int -> Int] -> [Int] -> [[Int]]
aplicaFunc [] _ = []
aplicaFunc (x:xs) y = [x z| z <- y] : aplicaFunc xs y

data DiaSemana = Domingo | Segunda | Terça | Quarta | Quinta | Sexta | Sabado deriving (Show, Ord ,Eq,Enum)
util :: DiaSemana -> Bool
util dia = dia > Domingo && dia < Sabado

ordenaDias :: [DiaSemana] -> [DiaSemana]
ordenaDias x = mergeSort x

ordenaDiasUteis :: [DiaSemana] -> [DiaSemana]
ordenaDiasUteis x = mergeSort(filter util x)

dataIgual :: [(DiaSemana, Int)] -> DiaSemana -> [Int]
dataIgual x y = [(snd z) | z <- x, (fst z) == y]

imprimeMes :: DiaSemana -> [(Int, DiaSemana)]
imprimeMes x = zip [1..30] ([x..Sabado] ++ (cycle[Domingo .. Sabado]))


