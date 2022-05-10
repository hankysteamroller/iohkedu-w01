module W0104 where

import qualified Data.Map as M
import           Prelude  hiding (lookup, null, traverse)

data Trie a = Fork (Maybe a) (M.Map Char (Trie a))
    deriving (Show, Eq)

example = Fork Nothing
  (M.fromList
    [('b', Fork Nothing
             (M.fromList [('a', Fork Nothing
                                (M.fromList [('r', Fork (Just 2) M.empty)
                                          ,('z', Fork (Just 3) M.empty)
                                          ]
                                )
                        )
                       ]
             )
     )
    ,('f', Fork (Just 0)
             (M.fromList [('o', Fork Nothing
                                (M.fromList [('o', Fork (Just 1) M.empty)])
                        )
                       ]
             )
     )
    ]
  )

example2 = Fork Nothing
  (M.fromList
    [('b', Fork Nothing
             (M.fromList [('a', Fork Nothing
                                (M.fromList [('r', Fork (Just 2) M.empty)
                                          ,('z', Fork (Just 3) M.empty)
                                          ]
                                )
                        )
                       ]
             )
     )
    ,('f', Fork (Just 0) (M.fromList []))
    ]
  )

empty  :: Trie a -- produces an empty trie
empty = Fork Nothing M.empty

null   :: Trie a -> Bool -- checks if a trie is empty
null (Fork Nothing m) = M.null m
null _                = False

-- We call a trie _valid_ if all its subtries are non-empty and valid.
valid  :: Trie a -> Bool -- checks if a trie adheres to the invariant
valid t = if null t then True else go t
  where
      go (Fork Nothing m) = M.null m == False && (allValuesInMapAre valid m)
      go (Fork _ m)       = allValuesInMapAre valid m

      allValuesInMapAre :: (a -> Bool) -> M.Map k a -> Bool
      allValuesInMapAre f m = all (==True) $ M.map f m

-- inserts/overwrites a key-value pair
insert :: String -> a -> Trie a -> Trie a
-- when input string is exhausted, insert/overwrite the value
insert [] v (Fork _ m) = Fork (Just v) m
insert (x:xs) v (Fork v' m) = case M.lookup x m of
  -- next char from string is not in trie so add a new fork
  (Nothing)      -> Fork v' (M.insert x (insert xs v empty) m)
  --next char from string has a place in trie, keep adding to existing fork
  (Just subtrie) -> insert xs v subtrie

lookup :: String -> Trie a -> Maybe a -- looks up the value associated with the key
lookup [] (Fork v _)     = v
lookup (x:xs) (Fork _ m) = case M.lookup x m of
     (Nothing)      -> Nothing
     (Just subtrie) -> lookup xs subtrie

delete :: String -> Trie a -> Trie a -- deletes the key if it exists
delete str t = case lookup str t of
  Nothing -> t
  _ -> go str t
    where
      go [] t'          = t'
      go [x] (Fork v m) = Fork v (clearRootOf x m)
      go xs (Fork v m)  = Fork v (deleteFromMap xs m)

      clearRootOf :: Char -> (M.Map Char (Trie a)) -> (M.Map Char (Trie a))
      clearRootOf c m = case (clearRoot $ M.lookup c m) of
        t' | null t' -> M.delete c m
        t'           -> M.insert c t' m

      deleteFromMap :: String -> (M.Map Char (Trie a)) -> (M.Map Char (Trie a))
      deleteFromMap (x:xs) m = case (go xs $ getTrie x m) of
        t' | null t' -> M.delete x m
        t'           -> M.insert x t' m

      clearRoot :: Maybe (Trie a) -> Trie a
      clearRoot Nothing           = empty
      clearRoot (Just (Fork _ m)) = Fork Nothing m

      getTrie :: Char -> (M.Map Char (Trie a)) -> Trie a
      getTrie c m = case M.lookup c m of
        Nothing   -> empty
        (Just t') -> t'

-- More generic solution here..
run :: String -> (Trie a -> Trie a) -> Trie a -> Trie a -- run computation for Trie specified by String and ensure validity
run [] _ t = t
run [x] computation (Fork v m) = case M.lookup x m of
  Nothing  -> run [] computation (Fork v m)
  (Just t) ->  case M.insert x (computation t) m of
        -- If some Trie is empty (ie. not valid) after modification, assume it's due to the computation and drop the Trie
        m' | (any (==True) $ M.map null m') -> Fork v (M.delete x m)
        m'                                  -> Fork v m'
-- for interim forks the computation is to recurse to next fork with substring
run (x:xs) computation (Fork v m) = run [x] (\ t' -> run xs computation t') (Fork v m)

delete' :: String -> Trie a -> Trie a
delete' xs = run xs (\ (Fork _ m) -> Fork Nothing m)
