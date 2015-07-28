module Tasks1 where

-- #1
myLast :: [a] -> a
myLast list = case list of [] -> error "There is no last element for empty list"
                           [x] -> x
                           (_:xs) -> myLast xs

-- #2
myButLast :: [a] -> a
myButLast [] = error "No one but last element for empty list"
myButLast (_:[]) = error "No one but last element for single element list"
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

-- #7
data NestedList a = Elem a | List [NestedList a] deriving (Show)

myFlatten :: NestedList a -> [a]
myFlatten (Elem a) = [a]
myFlatten (List []) = []
myFlatten (List (x:xs)) = myFlatten x ++ myFlatten (List xs)

-- #8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [a] = [a]
compress (a:b:xs) 
    | a == b = compress (b:xs)
    | otherwise = a : (compress (b:xs))

-- #9
pack :: (Eq a) => [a] -> [[a]]
pack [a] = [[a]]
pack list = packWithAccumulator list []
    where packWithAccumulator :: (Eq a) => [a] -> [a] -> [[a]]
          packWithAccumulator [a] acc = [a:acc]
          packWithAccumulator (a:b:xs) acc
              | a == b = packWithAccumulator (b:xs) (a:acc)
              | otherwise = (a:acc) : (packWithAccumulator (b:xs) [])

-- #10
encode :: (Eq a, Num b) => [a] -> [(b,a)]
encode [] = error "Encode cannot be performed on empty list"
encode list = encodeWithCount list 1
    where encodeWithCount :: (Eq a, Num b) => [a] -> b -> [(b,a)]
          encodeWithCount [a] count = [(count, a)]
          encodeWithCount (a:b:xs) count
              | a == b = encodeWithCount (b:xs) (count+1)
              | otherwise = (count, a) : (encodeWithCount (b:xs) 1)

