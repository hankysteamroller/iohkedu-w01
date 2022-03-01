module W0104 where

import Data.Map (Map)

data Trie a = Fork (Maybe a) (Map Char (Trie a))

empty  :: Trie a -- produces an empty trie
empty = error "TODO: implement empty"

null   :: Trie a -> Bool -- checks if a trie is empty
null = error "TODO: implement null"

valid  :: Trie a -> Bool -- checks if a trie adheres to the invariant
valid = error "TODO: implement valid"

insert :: String -> a -> Trie a -> Trie a -- inserts/overwrites a key-value pair
insert = error "TODO: implement insert"

lookup :: String -> Trie a -> Maybe a -- looks up the value associated with the key
lookup = error "TODO: implement lookup"

delete :: String -> Trie a -> Trie a -- deletes the key if it exists
delete = error "TODO: implement delete"
