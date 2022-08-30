import Data.Char
import Data.List

-- Quesito 1

mult a b = if a > 0 && b > 0 || a < 0 && b < 0 then res else negate res
    where
        res = sum (replicate (abs a) (abs b))

soma x y = x + y

-- Outra forma de fazer a questão

xor True True = False
xor False False = False
xor _ _ = True

mult2 _ 0 = 0
mult2 0 _ = 0
mult2 x y
  | xor (x < 0) (y < 0) = negate res
  | otherwise = res
  where
    res = soma (abs y) (mult2 (abs x - 1) (abs y))

-- Quesito 2

raiz6 1 = sqrt 6
raiz6 a = sqrt (6 + raiz6 (a -1))

-- Quesito 3

maiorDaLista l = maiorDaListaAux (tail novaLista) (head novaLista)
  where
    novaLista = zip l [0 ..]

maiorDaListaAux [] maiorAtual = maiorAtual
maiorDaListaAux (x : xs) maiorAtual =
  if fst x > fst maiorAtual
    then maiorDaListaAux xs x
    else maiorDaListaAux xs maiorAtual

-- Outro jeito

maiorDaLista2 l = maiorDaListaAux2 (tail l) (head l, 0) 0

maiorDaListaAux2 [] tupla i = tupla
maiorDaListaAux2 (x : xs) tupla i =
  if x > fst tupla
    then maiorDaListaAux2 xs (x, i + 1) (i + 1)
    else maiorDaListaAux2 xs tupla (i + 1)

-- Quesito 4

conv [] = []
conv (n : ns) = dic10 !! n : conv ns
  where
    dic10 = ["Zero", "Um", "Dois", "Tres", "Quatro", "Cinco", "Seis", "Sete", "Oito", "Nove", "Dez"]

-- Outro Jeito
dic10 = ["Zero", "Um", "Dois", "Tres", "Quatro", "Cinco", "Seis", "Sete", "Oito", "Nove", "Dez"]

conv2 [] = []
conv2 l = map (dic10 !!) l

-- Quesito 5

delPosicaoN :: [Int] -> Int -> [Int]
delPosicaoN a b = take b a ++ drop (b + 1) a

-- Quesito 6

inserirPosicaoN :: [Int] -> Int -> Int -> [Int]
inserirPosicaoN l n v = take n l ++ (v : drop n l)

-- Quesito 7

-- Quesito 8

-- Questão 9

interseccao l1 l2 = [x | x <- l1, elem x l2]

-- Questão 10

comprime :: String -> String
comprime s = concat [comp x | x <- group s]

comp :: String -> String
comp s
  | length s > 3 = "!" ++ show (length s) ++ [head s]
  | otherwise = s

-- Questão 11 takeWhile isDigit

--descompacta :: String -> String
--descompacta s = desc

-- desc s =