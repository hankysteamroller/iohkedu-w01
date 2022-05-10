module W0101 where

import qualified Data.List as L
-- | Returns the list of all permutations of the argument
--
-- >>> import qualified Data.List as L
-- >>> L.sort (permutations "abc")
-- ["abc","acb","bac","bca","cab","cba"]
--
permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = do
    x <- xs
    let rest = L.delete x xs
    restP <- permutations rest
    return $ x : restP

listMonadSum :: Num a => [a] -> [a] -> [a]
listMonadSum xs ys = do
    x <- xs
    y <- ys
    return $ x + y
