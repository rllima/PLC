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
troca l x y = (take (x-1) l) ++(take 1(drop (y-1) l)) ++ (take(y-x-1)(drop x l)) ++ (take 1(drop (x-1) l)) ++ (drop y l)

type Codigo = Int
data Voto = Presidente Codigo | Conselheiro Codigo | Secretario Codigo | Branco deriving (Show)
type Urna = [Voto]
type Apuraçao = [(Voto, Int)]
instance Eq Voto where
  (Presidente a)==(Presidente b) = a==b
  (Conselheiro a)==(Conselheiro b) = a==b
  (Secretario a)==(Secretario b) = a==b
  (Branco)==(Branco) = True
  _== _ = False

totalVotos :: Urna -> Voto -> Int
totalVotos a b = length([x | x <-a, x==b])

apurar :: Urna -> Apuraçao
apurar[] = []
apurar l@(x:xs) = (x, totalVotos l x) : apurar (filter (/=x) xs)

data Pilha t = Pilha [t] | PilhaVazia deriving (Show)

push :: t -> Pilha t -> Pilha t
push t PilhaVazia = Pilha [t]
push t (Pilha a) = (Pilha (t:a))

pop :: Pilha t -> Pilha t
pop PilhaVazia = error "Pilha Vazia"
pop (Pilha (x:xs)) = (Pilha xs)

top :: Pilha t -> t
top PilhaVazia = error "Pilha Vazia"
top (Pilha (x:xs)) = x

data Btree = Leaf | Node (Btree) Int  (Btree) deriving (Show)

insert :: Int -> Btree -> Btree
insert y Leaf  = Node Leaf y Leaf
insert y (Node xl k xr)
 |y==k = (Node xl k xr)
 |y < k = Node (insert y xl) k xr
 |y > k = Node xl k (insert y xr)

arvLista :: Btree -> [Int]
arvLista Leaf = []
arvLista (Node xl k xr) = arvLista xl ++ [k] ++ arvLista xr

somaArv :: Btree -> Int
somaArv Leaf = 0
somaArv (Node xl k xr) = somaArv xl + k + somaArv xr

listaArv :: [Int] -> Btree
listaArv [] = Leaf
listaArv (x:xs) = Node (listaArv (filter (<x) xs)) x (listaArv (filter(>x)xs))

quick :: (Ord a)=> [a] -> [a]
quick[] = []
quick (x:xs) = (quick (filter(<=x)xs)) ++ [x] ++ (quick(filter(>x)xs))
