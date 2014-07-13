-- #1
myLast :: [a] -> a
myLast list = case list of [] -> error "There is no last element for empty list"
                           (elem:[]) -> elem
                           (_:xs) -> myLast xs

-- #2
myButLast :: [a] -> a
myButLast [] = error "No one but last element for empty list"
myButLast [elem] = error "No one but last element for single element list"
myButLast (a:_:[]) = a
myButLast (_:xs) = myButLast xs

-- #3
elementAt :: (Integral b) => [a] -> b -> a
elementAt (x:xs) index
    | index > 1 = elementAt xs (index - 1)
    | otherwise = x
elementAt [] 1 = error "Index out of bounds"

-- #4
myLength :: (Num b) => [a] -> b
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- #5
myReverse :: [a] -> [a]
myReverse (elem:[]) = elem:[]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ (x:[])

-- #6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome (_:[]) = True
isPalindrome (x:xs) = (x == last xs) && isPalindrome (init xs)

-- #8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [a] = [a]
compress (a:b:xs) 
    | a == b = compress (b:xs)
    | otherwise = a : (compress (b:xs))

