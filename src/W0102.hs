module W0102 where

-- |  The idea of merge sort is to
--    split a list into two more or less equal parts, sort them recursively,
--    and then merge two sorted lists using a dedicated merge function.
--
-- >>> mergesort [6,5,3,1,8,7,2,4]
-- [1,2,3,4,5,6,7,8]
--
mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs =
    let
        (as, bs) = split xs
    in
        merge (mergesort as) (mergesort bs)

-- | Merges two (sorted) lists.
--
-- >>> merge "ADEX" "ABF"
-- "AABDEFX"
--
merge :: Ord a => [a] -> [a] -> [a]
merge xs ys = go xs ys []
    where
        go :: Ord a => [a] -> [a] -> [a] -> [a]
        go [] [] acc = acc
        go as [] acc = acc ++ as
        go [] bs acc = acc ++ bs
        go (a:as) (b:bs) acc = if a <= b then go as (b:bs) (acc ++ [a]) else go (a:as) bs (acc ++ [b])

-- | Splits a list into two lists of almost equal length.
--
split :: [a] -> ([a], [a])
split xs = (take headLen xs, drop headLen xs)
    where
        headLen = length xs `div` 2
