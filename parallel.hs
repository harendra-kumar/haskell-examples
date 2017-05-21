-- ghc -rtsopts -threaded parsort.hs
-- ./parsort +RTS -s -N4

import System.Random
import Control.Parallel.Strategies
import Data.List

main = do
    g <- getStdGen

    let
        chunks :: [[Double]]
        chunks = (replicate 8 $ take 100000 (randoms g))

    -- print $ sum
    --     $ map (sum . sort) chunks

    print $ sum
          $ (parMap rpar) (sum . sort) chunks
