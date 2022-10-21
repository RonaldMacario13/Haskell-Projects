import System.Win32 (COORD(y))
-- Exemplo 1

div :: (Integral a, Fractional a) => a -> a -> Maybe a
div x y
    | y == 0 = Nothing
    | otherwise = Just (x/y)

-- Exeplo 2

data Arvore a = Folha a | No (Arvore a) a (Arvore a)

-- No (No (Folha 1) 3 (Folha 4)) 5 (No (Folha 6) 7 (Folha 9))

-- Exemplo 3

existe :: Eq a => a -> Arvore a -> Bool
existe x (Folha y) = x == y
existe x (No esq y dir) = x == y || existe x esq || existe x dir

-- Exemplo 4

serializar :: Arvore a -> [a]
serializar (Folha x) = [x]
serializar (No e x d) = serializar e ++ [x] ++ serializar d

-- Exemplo 5 

existe1 :: Ord a => a -> Arvore a -> Bool
existe1 x (Folha y) = x == y
existe1 x (No esq y dir)
    | x == y    = True
    | x < y     = existe1 x esq
    | otherwise = existe1 x dir

{- Exemplo 6

class Eq a where
    (==), (/=) :: a -> a -> Bool
    x /= y = not (x == y)

instance Eq Bool where
    False == False = True
    True == True = True
    _ == _ = False
-}
 