all2 _ [] = True
all2 f (x:xs) = f x && all2 f xs

all4 f xs = foldl (&&) True (map f xs)

-- foldl (\_ n -> 1 + n) 0 [1,2,3]
