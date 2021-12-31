module List
    (find, split, remove) 
where


-- find element in list
find :: [a] -> (a -> Bool) -> Maybe a
find [] _ = Nothing 
find (a:as) fn 
    | fn a = Just a
    | otherwise = find as fn



-- split list by element in a tuple
split :: [a] -> (a -> Bool) -> ([a] , [a])
split [] _ = ([], [])
split (a:as) fn
    | fn a =
         let (x, y) = split as fn
         in (a : x, y)
    | otherwise = ([], a:as)


-- removes character from string
remove :: Eq a => [a] -> a ->  [a]
remove [] _ = []
remove (a:as) c 
    | a == c = remove as c
    | otherwise = a : remove as c
