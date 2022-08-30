import Distribution.PackageDescription (ConfVar(Flag))
{-
    Questão 1: Como a versão recursiva da função fatorial se comporta se dermos a ela como
    argumento um número negativo? Modifique a implementação clássica para não permitir
    números negativos adicionando uma guarda ao passo recursivo.
-}

fatorial :: Int -> Int
fatorial n = product [1 .. n]

fatorial' :: Int -> Int
fatorial' 0 = 1
fatorial' n = n * fatorial (n - 1)

--fatorial'' :: Int -> Int
fatorial'' n 
    |n < 0 = error "Números negativos não são permitidos."
    |n == 0 = 1
    |otherwise = n * fatorial'' (n - 1)

{-
    Questão 2: Defina a função recursiva somar :: Int -> Int que retorna a soma dos inteiros
    não-negativos a partir de um valor até zero. Por exemplo, somar 3 deve retornar 3+2+1+0 = 6.
-}

somar :: Int -> Int
somar 0 = 0
somar n = n + somar (n - 1)

{-
    Questão 3: Defina o operador de exponenciação ^ utilizando uma função recursiva, semelhante
    ao padrão usado para implementar a multiplicação com o operador *:
    (*) :: Num a => a -> a -> a
    m * 0 = 0
    m * n = m + (m * (n - 1))
-}

(^^^) :: (Eq a, Num a) => a -> a -> a
n ^^^ 0 = 1
n ^^^ 1 = n
n ^^^ k = n * (n ^^^ (k - 1))

{-
    Questão 4: Defina a função euclides :: Int -> Int -> Int que implementa o algoritmo de Euclides
    para calcular o máximo divisor comum de dois inteiros não-negativos: se dois números são iguais,
    este número é o resultado; caso contrário, o menor número é subtraído do maior e o processo é
    repetido passando este novo número e o menor valor passado anteriormente como argumento. Exemplo:
    > euclides 6 27
    3
-}

euclides' n m = if n > m then resto n m else resto m n
                    where
                        resto maior menor =if maior `mod` menor == 0 then menor else euclides' (maior - menor) menor

euclides :: Int -> Int -> Int
euclides n k
    |n == k = n
    |n > k = euclides (n - k) k
    |otherwise = euclides (k - n) n

{-
    Questão 5: Defina as funções abaixo usando recursão:
        a) Decidir se todos os valores em uma lista são True:
            and :: [Bool] -> Bool
-}


and' :: [Bool] -> Bool
and' [] = True
and' (n:ns)
    |n == False = False
    |otherwise = n && and' ns
{-
        b) Concatenar uma lista de listas:
            concat :: [[a]] -> [a]
-}
concat' :: [[a]] -> [a]
concat' [] = []
concat' (n:ns) = n ++ concat' ns
{-
        c) Produzir uma lista com n elementos idênticos:
            replicate :: Int -> a -> [a]
-}
replicate' :: (Eq a, Num a) => Int -> a -> [a]
replicate' _ 0 = []
replicate' n k = replicate' (n - 1)  k
{-
        d) Selecionar o n-ésimo elemento em uma lista:
            (!!) :: [a] -> Int -> a
-}

{-
        e) Decidir se um valor está presente em uma lista:
            elem :: Eq a => a -> [a] -> Bool
-}