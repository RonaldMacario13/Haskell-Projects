-- Aula de listas

-- Divisores de n de 1 até n

fatores :: Int -> [Int]
fatores n = [x | x <- [1 .. n], mod n x == 0]

-- Saber se número é primo

primo :: Int -> Bool
primo n = fatores n == [1,n]

-- Mostrar números primos até n

primos :: Int -> [Int]
primos n = [x | x <- [2 .. n], primo x]

-- 

buscar :: Eq a => a -> [(a,b)] -> [b]
buscar k xs = [v | (k', v) <- xs, k == k']

pares :: [a] -> [(a, a)]
pares xs = zip xs (tail xs)

--

ordenada :: Ord a =>  [a] -> Bool
ordenada xs = length [x | (x, y) <- pares xs, x > y] > 0
