values a b c =
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

maioresQueMedia :: (Num a1, Ord a2, Fractional a2) => a2 -> a2 -> a2 -> a1
maioresQueMedia a b c = maior a media + maior b media + maior c media
  where
    media = (a + b + c) / 3

maior x y = if x > y then 1 else 0

------------------

nomeInt :: Int -> String
nomeInt x
  | x == 0 = "Zero"
  | x == 1 = "Um"