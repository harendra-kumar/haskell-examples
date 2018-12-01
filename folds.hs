import Data.Foldable

main = do
    ---------------------------------------------------------------------------
    -- Reductions (foldr)
    ---------------------------------------------------------------------------

    -- output can only be a function of head or 0
    -- not very useful
    print $ foldr (\x _ -> x) 0 [1..1000000 :: Int]

    -- output is the supplied zero element
    -- useless
    print $ foldr (\_ xs -> xs) ([] :: [Int]) [1..1000000 :: Int]

    -- consumes all input before reduction
    print $ foldr (\x xs -> x + xs) 0 [1..1000000 :: Int]

    ---------------------------------------------------------------------------
    -- Reductions (foldl)
    ---------------------------------------------------------------------------

    -- output is the initial value of the accumualtor
    -- useless
    print $ foldl (\xs _ -> xs) (0 :: Int) [1..1000000 :: Int]

    -- output is the last value of the input or 0
    -- not very useful
    print $ foldl (\_ x -> x) (0 :: Int) [1..1000000 :: Int]

    -- reduces efficiently
    print $ foldl (\xs x -> x + xs) (0 :: Int) [1..1000000 :: Int]

    ---------------------------------------------------------------------------
    -- Constructions (foldr)
    ---------------------------------------------------------------------------

    -- get the head
    print $ foldr (\x _ -> Just x) Nothing [1..1000000 :: Int]

    -- useless
    -- print $ foldr (\_ xs -> xs) ([] :: [Int]) [1..1000000 :: Int]

    print $ foldr (\x xs -> x : xs) [] [1..10 :: Int]

    ---------------------------------------------------------------------------
    -- Constructions (foldl)
    ---------------------------------------------------------------------------

    -- useless
    -- print $ foldl (\xs _ -> xs) (0 :: Int) [1..1000000 :: Int]

    -- get last
    print $ foldl (\_ x -> Just x) Nothing [1..1000000 :: Int]

    -- reverse
    print $ foldl (\xs x -> x : xs) [] [1..10 :: Int]
