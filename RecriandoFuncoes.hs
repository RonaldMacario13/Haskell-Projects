filter2 :: (a -> Bool) -> [a] -> [a]
filter2 _ [] = [] 
filter2 f (x:xs) = if f x then x:filter2 f xs
                        else filter2 f xs