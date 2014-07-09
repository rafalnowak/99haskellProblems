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
