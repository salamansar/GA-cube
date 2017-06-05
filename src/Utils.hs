module Utils where


expandElems :: (Int->a) -> Int -> [a]
expandElems f size = [f count | count <- [0..size]]

zeros :: Int -> [Int]
zeros size = expandElems (\x -> 0) size
                                   
gatherElems :: [(a,a)] -> [a]
gatherElems elems = let (left, right) = unzip elems in left ++ right
                                   
 
