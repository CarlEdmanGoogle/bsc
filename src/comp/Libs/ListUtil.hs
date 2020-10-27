-- Copyright (c) 1982-1999 Lennart Augustsson, Thomas Johnsson
-- See LICENSE for the full license.
--

module ListUtil where

mapFst :: (a -> c) -> [(a, b)] -> [(c, b)]
mapFst f xys = [(f x, y) | (x, y) <- xys]
mapSnd :: (b -> c) -> [(a, b)] -> [(a, c)]
mapSnd f xys = [(x, f y) | (x, y) <- xys]

-- Drop repeated (adjacent) elements according to a predicate
dropRepeatsBy :: (a -> a -> Bool) -> [a] -> [a]
dropRepeatsBy _ []  = []
dropRepeatsBy _ [x] = [x]
dropRepeatsBy fn (x:y:rest) | fn x y = dropRepeatsBy fn (x:rest)
                            | otherwise = x:(dropRepeatsBy fn (y:rest))

-- Given a sorting predicate and two lists sorted accordingly,
-- returns the list of all elements in the first list and not in the second
listDifference :: (a -> a -> Bool) -> [a] -> [a] -> [a]
listDifference p = go
  where
    -- p a b <=> a > b
    go aa@(a:as) ba@(b:bs)
      | p a b     = go aa bs
      -- Allow single element in b to filter multiple elements in a
      | otherwise = go as ba
    go aa _ = aa

-- Given a sorting predicate and a list sorted accordingly,
-- returns list with all duplicates removed
listDedupe :: (a -> a -> Bool) -> [a] -> [a]
listDedupe p (e:es)= go e es
  where
    -- p a b <=> a == b || a somehow includes b
    go a1 (a2:as)
      | p a1 a2   = go a1 as
      | otherwise = a1:go a2 as
    go a1 _ = [a1]
listDedupe _ _ = []

