-- QUESITO 1

values x y z
    | x == z && x == y = 3
    | x == z || x == y || y == z = 2
    | otherwise = 0



-- QUESITO 2

values2 a b c =
  if a > ((a + b + c) / 3) && b > ((a + b + c) / 3) && c > ((a + b + c) / 3)
    then 3
    else
      if a > ((a + b + c) / 3) && b > ((a + b + c) / 3) || a > ((a + b + c) / 3) && c > ((a + b + c) / 3) || b > ((a + b + c) / 3) && c > ((a + b + c) / 3)
        then 2
        else
          if a > ((a + b + c) / 3) || b > ((a + b + c) / 3) || c > ((a + b + c) / 3)
            then 1
            else 0

-- Resolução do professor.

maioresQueMedia a b c = maior a media + maior b media + maior c media
  where
    media = (a + b + c) / 3

maior x y = if x > y then 1 else 0

-- QUESITO 3

potencia2 x = x * x
-- QUESITO 4

potencia2 x = x * x

potencia4 x = potencia2 x * potencia2 x

-- QUESITO 5

xor a b = (a || b) && not (a && b)

-- QUESITO 6

delta a b c = (b * b) - ((4 * a) * c)

raiz1 a b c = ((- b) + sqrt (delta a b c)) / (2 * a)

raiz2 a b c = ((- b) - sqrt (delta a b c)) / (2 * a)

--maiormenor a b c = if (raiz1 a b c) > (raiz2 a b c)
--                        then raiz1 a b c
--                            else raiz2 a b c
-- Jeito do professor

xMenor a b c = if x1 > x2 then x2 else x1
  where
    x1 = raiz1 a b c
    x2 = raiz2 a b c

xMaior a b c = if x1 < x2 then x2 else x1
  where
    x1 = raiz1 a b c
    x2 = raiz2 a b c
{-
-- QUESITO 7

somaInclusivo n1 n2 = sum [n1 .. n2]

somaExclusivo n1 n2 = sum [(n1 + n2) .. (n1 - n2)]

-- QUESITO 8

multiplos n1 n2 n3 = length [x | x <- [n1 .. n2], mod x n3 == 0]

-- QUESITO 9

fat 0 = 1
fat 1 = 1
fat n = n * fat (n - 1)

-- QUESITO 10

-- mod2 n d =

-}