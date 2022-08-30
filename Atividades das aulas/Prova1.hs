{- QUESTÃO 1
Fornecidos três valores a, b e c, escreva uma função que retorne quantos dos três são iguais.
A resposta pode ser 3 (todos iguais), 2 (dois iguais e o terceiro diferente) ou 0 (todos diferentes).
-}

iguais a b c
    |a == b && b == c = 3
    |a == b || a == c || c == b = 2
    |otherwise = 0

{- QUESTÃO 2
Fornecidos três valores a, b e c, elaborar uma função que retorne quantos desses três valores
são maiores que a média entre eles.
-}

maiorMedia a b c
    |a > media && b > media && c > media = 3
    |a > media && b > media || b > media && c > media || a > media && c > media = 2
    |a > media || b > media || c > media = 1
    |otherwise = 0
        where media = (a + b + c)/3

-- Outra forma de fazer

maiorMedia2 a b c = maior a media + maior b media + maior c media
    where media = (a + b + c) / 3

maior a b = if a > b then 1 else 0

{- QUESTÃO 3
Escreva uma função potencia_2 que retorne o quadrado de um número (x2).
-}

potencia_2 a = a * a

{- QUESTÃO 4
Reutilizando a função potencia_2, construir uma função potencia_4 que retorne
o seu argumento elevado à quarta potência.
-}

potencia_4 a = potencia_2 (potencia_2 a)

{- QUESTÃO 5
Implemente em Haskell a função do ou-exclusivo, que é dada por:
a (x) b = (a V b) ^ -(a ^ b)
-}

xor a b = (a || b) && not (a && b)

{- QUESTÃO 6
Escrever duas funções, x_maior que retorne o maior e x_menor que retorne
o menor valor real, das raízes de uma equação do segundo grau.
A expressão genérica é dada por: Bháskara.
-}

raiz1 a b c = ((- b) + sqrt ((b * b) - (4 * a * c))) / (2 * a)
raiz2 a b c = ((- b) - sqrt ((b * b) - (4 * a * c))) / (2 * a)

x_maior a b c = if x1 > x2 then x1 else x2
    where
        x1 = raiz1 a b c
        x2 = raiz2 a b c

x_menor a b c = if x1 < x2 then x1 else x2
    where
        x1 = raiz1 a b c
        x2 = raiz2 a b c