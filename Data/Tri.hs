module Data.Tri where

import Helpers

data Tri a = Split !Int (Tri a) (Tri a) (Tri a)
           | Leaf a
           deriving (Eq, Show, Foldable)

size :: Tri a -> Int
size (Split n _ _ _) = n
size (Leaf _)        = 1

toList :: Tri a -> [a]
toList t = go t []
  where go (Leaf x) acc = x : acc
        go (Split _ t1 t2 t3) acc = go t1 (go t2 (go t3 acc))

fromList :: [a] -> Tri a
fromList [x] = Leaf x
fromList ls  = Split n (fromList x) (fromList y) (fromList z)
  where
    n = length ls
    [x, y, z] = bundle (n `div` 3) ls

(!) :: Tri a -> Int -> a
(Leaf x) ! _ = x
(Split p t1 t2 t3) ! i
  | i < n     = t1 ! i
  | i < 2*n   = t2 ! (i - n)
  | otherwise = t3 ! (i - 2*n)
  where n = p `div` 3

update :: Int -> a -> Tri a -> Tri a
update _ x (Leaf _) = Leaf x
update i x (Split p t1 t2 t3)
  | i < n     = Split p (update i x t1) t2 t3
  | i < 2*n   = Split p t1 (update (i - n) x t2) t3
  | otherwise = Split p t1 t2 (update (i - 2 * n) x t3)
  where n = p `div` 3

-- For your debugging purposes
errorBadIndex :: Int -> Int -> a
errorBadIndex i sz =
  error ("index " ++ show i ++ " out of bounds for Tri of size " ++ show sz)

{-|
Pretty prints a tri very tidily:

ghci> printTri (fromList [0..8])

├──┐
│  ├──0
│  ├──1
│  └──2
├──┐
│  ├──3
│  ├──4
│  └──5
└──┐
   ├──6
   ├──7
   └──8
-}
printTri :: Show a => Tri a -> IO ()
printTri t = putStrLn (drop 1 (pretty True [] t "")) where
  pretty end bars (Leaf x)           = showsBars (node end) bars
                                     . shows x
                                     . showString "\n"
  pretty end bars (Split _ t1 t2 t3) = showsBars (node end) bars
                                     . showString "┐\n"
                                     . pretty False (withBar bars) t1
                                     . pretty False (withBar bars) t2
                                     . pretty True (withEmpty bars) t3

  node True = "└──"
  node False = "├──"

  showsBars :: String -> [String] -> ShowS
  showsBars node [] = id
  showsBars node (_ : tl) =
    showString (concat (reverse tl)) . showString node

  withBar, withEmpty :: [String] -> [String]
  withBar bars   = "│  ":bars
  withEmpty bars = "   ":bars
