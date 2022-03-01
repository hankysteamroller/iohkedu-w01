module W0103 where

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

splitleft :: Tree a -> (a, Maybe (Tree a))
splitleft (Leaf a  ) = (a, Nothing)
splitleft (Node l r) = case splitleft l of
  (a, Nothing) -> (a, Just r)
  (a, Just l') -> (a, Just (Node l' r))

-- | A tail-recursive version of @'splitleft'@.
splitleft' :: Tree a -> (a, Maybe (Tree a))
splitleft' = error "TODO: implement splitleft'"
