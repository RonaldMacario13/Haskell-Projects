-- QUESITO 1

duasVezes :: (t -> t) -> t -> t 
duasVezes f x = f (f x)

-- QUESITO 2
-- A)

terceiro :: [a] -> a
terceiro xs
    |length xs < 3 = error "Não pode conter menos que 3 elementos"
    |otherwise = head(tirar2 xs)

tirar2 xs = tail(tail xs)

-- B)

terceiro' :: [a] -> a
terceiro' xs
    |length xs < 3 = error "Não pode conter menos que 3 elementos"
    |otherwise = xs !! 2

-- C)

terceiro'' :: [a] -> a
terceiro'' xs
    |length xs < 3 = error "Não pode conter menos que 3 elementos"
    |otherwise = escolher3 xs

escolher3 (x:y:z:xs) = z

-- QUESTÃO 3
--A)



-- QUESITO 4

reverso :: [a] -> [a]
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]


-- QUESITO 5

fatores :: Int -> [Int]
fatores n = [x | x <- [1 .. n], mod n x == 0]

primo :: Int -> Bool
primo n = fatores n == [1,n]

primos :: Int -> [Int]
primos n = [x | x <- [2 .. n], primo x]

-- QUESITO 6

shiftLeft :: Int -> [a] -> [a]
shiftLeft z (x:xs) = if z == 0 then x:xs else shiftLeft (z - 1) (xs ++ [x])
--     |z == 0 = x:xs
--     |otherwise = shiftLeft (z - 1) (xs ++ [x])

-- shiftLeft 0 xs = xs
-- shiftLeft z (x:xs) = shiftLeft (z - 1) (xs ++ [x])
