quicksort [] = []
quicksort (x:xs) =
    quicksort menores ++ [x] ++ quicksort maiores
        where
            menores = [e | e <-xs, e < x]
            maiores = [e | e <- xs, e >= x]


-- imparesOrd x = sort(filter odd x)