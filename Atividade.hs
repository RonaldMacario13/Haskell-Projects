-- Quetão 1

soma n = sum [x * x | x <- [1 .. n]]

--Questão 2

grid m n = [(x, y) | x <- [0 .. m], y <- [0 .. n]]

-- Questão 3

quadrado :: Int -> [(Int, Int)]
quadrado n = [(x, y) | (x, y) <- grid n n, x /= y]

-- Questão 4

-- replicate :: Int -> a -> [a]
replicate' n z = [n | _ <- [1 .. z]]

-- Questão 5

pitag :: Int -> [(Int, Int, Int)]
pitag n = [(x, y, z) | x <- [3 .. n], y <- [3 .. n], z <- [3 .. n], (x * x) + (y * y) == (z * z)]

-- Questão 6

fatores n = [x | x <- [1 .. n], mod n x == 0]

--perfeitos :: Int -> [Int]
perfeitos n = [x | x <- [1 .. n], x == sum (init (fatores x))]

-- Questão 7

concat2 = [[(1, y) | y <- [3, 4]], [(2, y) | y <- [3, 4]]]

-- Questão 8


buscar k xs = [v | (k', v) <- xs, k == k']

posicoes x xs = buscar x (zip xs [0 ..])

-- Questão 9

produtoEscalar xs ys = sum [x * y| (x, y) <- zip xs ys]

produtoEscalar' xs ys = sum (zipWith (*) xs ys)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = [f x y | (x, y) <- zip xs ys]