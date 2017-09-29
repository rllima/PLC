fatN :: Int -> [Int]
fatN a = [product [1..b] | b <- [1..a]]

testaLista :: (a -> Bool) -> [a] -> Bool
testaLista f [] = True
testaLista f (y:ys) = (f y) && (testaLista f ys)

testaListM :: (a -> Bool) -> [a] -> Bool
testaListM f [] = True
testaListM f a = and (map f a)

testaListF :: (a -> Bool) -> [a] -> Bool
testaListF f [] = True
testaListF f x= foldr (\a b -> f a && b) True x

type Nome = String
type Conteudo = String
