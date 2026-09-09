module Helpers where

import Data.Containers.ListUtils (nubOrd)
import Data.Set (Set)
import Data.Set qualified as Set
import Control.Monad (guard)

mhead :: [a] -> Maybe a
mhead [] = Nothing
mhead (x:_) = Just x

bundle :: Int -> [a] -> [[a]]
-- Pre: n > 0
bundle n [] = []
bundle n xs = fst (splitAt n xs) : bundle n (snd(splitAt n xs)) -- TODO: part 1

unique :: Ord a => [a] -> Bool
unique xs = length xs == length (nubOrd xs)

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (x, y, z) = f x y z

getSingle :: Set a -> Maybe a
getSingle s = do guard (Set.size s == 1)
                 Set.lookupMin s
