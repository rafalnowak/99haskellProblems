module Tasks2 where

-- #14
dupli :: [a] -> [a]
dupli [] = []
dupli [x] = [x, x]
dupli (x:xs) = x : x : dupli xs

-- #15
repli :: [a] -> Int -> [a]
repli list n = do
    char <- list
    replicate n char

-- #16
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xss i = dropListWithCounter xss 1 []
    where dropListWithCounter :: [a] -> Int -> [a] -> [a]
          dropListWithCounter (x:xs) counter acc
            | counter `mod` i == 0 = dropListWithCounter xs (counter + 1) acc
            | otherwise = dropListWithCounter xs (counter + 1) (x : acc)
          dropListWithCounter [] _ acc = reverse acc

-- #17
split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split list@(x:xs) len 
    | len > 0 = (x : ys, zs)
    | otherwise = ([], list)
    where (ys, zs) = split xs (len - 1)

-- #19
rotate :: [a] -> Int -> [a]
rotate list 0 = list
rotate [] _ = []
rotate [x] _ = [x]
rotate (x:xs) r
    | r > 0 = rotate (xs ++ [x]) (r - 1)
    | r < 0 = rotate (x:xs) (r + len)
    where len = length (x:xs)

-- #20
removeAt :: Int -> [a] -> (a, [a])
removeAt x list = (list !! (x - 1), take (x - 1) list ++ drop x list)
