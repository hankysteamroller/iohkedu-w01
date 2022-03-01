module W0107 where

data GP a = End a
          | Get (Int -> GP a)
          | Put Int (GP a)

echo :: GP a
echo = Get (\n -> Put n echo)

run :: GP a -> IO a
run = error "TODO: implement run"

add :: GP ()
add = error "TODO: implement add"

accum :: GP Int
accum = error "TODO: implement accum"

simulate :: GP a -> [Int] -> (a, [Int])
simulate = error "TODO: implement simulate"

instance Functor GP where
    fmap = error "TODO: implement fmap"

instance Applicative GP where

    pure = error "TODO: implement pure"

    (<*>) = error "TODO: implement (<*>)"

instance Monad GP where

    return = error "TODO: implement return"

    (>>=) = error "TODO: implement (>>=)"
