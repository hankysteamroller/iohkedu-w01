{-# OPTIONS_GHC -Wno-orphans #-}

module W0104Spec
    ( spec
    ) where

import Test.Hspec
import Test.QuickCheck

import W0104

spec :: Spec
spec = return () -- TODO: implement at least three tests

instance Arbitrary a => Arbitrary (Trie a) where

    arbitrary = error "TODO: implement arbitrary"

    shrink = error "TODO: implement shrink"
