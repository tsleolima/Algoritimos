bubble :: (Ord a) => [a] -> [a]
bubble []     = []
bubble list  = bubbleOrd list (length list)

bubbleOrd :: (Ord a) => [a] -> Int -> [a]
bubbleOrd list 0 = list
bubbleOrd list n = bubbleOrd (change list) (n-1)

change :: (Ord a) => [a] -> [a]
change [x]     = [x]
change (x:y:zs) 
    | x > y      = y : change (x:zs)
    | otherwise  = x : change (y:zs)