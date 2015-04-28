{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module Combinators where

import SetFunctions ( foldValues, mapValues, valueOf
       		    , set0, set1, set3)

allDifferent :: Eq a => [a] -> Bool
allDifferent = allDifferentF id

allDifferentF :: Eq b => (a -> b) -> [a] -> Bool
allDifferentF f []     = True
allDifferentF f (x:xs) = all ((/= f x) . f) xs && allDifferentF f xs

has :: Eq b => a -> (a -> b) -> b -> Bool
has val f valB = foldValues (&&) True $ mapValues (`valueOf` set1 f val) (set0 valB)

hasNot :: Eq b => a -> (a -> b) -> b -> Bool
hasNot val f = not . has val f

with :: (a -> Bool) -> [a] -> Bool
with f xs = all f xs

infixl 1 |>

different = (/=)
same = (==)

have_ :: Eq b => [a] -> (b -> b -> Bool) -> (a -> b) -> Bool
have_ []     _    _   = True
have_ (x:xs) pred sel = all (pred (sel x) . sel) xs && have_ xs pred sel


have :: (Eq a, Ord b) => [a] -> (b -> b -> Bool) -> (a -> b) -> Bool
have xs pred sel = foldValues (&&) True (set3 have' (zip xs [1..]) pred sel)

have' ys pred sel | n /= m = pred (sel x) (sel y)
   where
    (x,n) = oneOf ys
    (y,m) = oneOf ys

are :: Ord a => [a] -> (a -> a -> Bool) -> Bool
are xs pred = have xs pred id

asA  = flip
asAn = flip

oneOf :: [a] -> a
oneOf (x:_) = x
oneOf (_:xs) = oneOf xs

suchThat' :: a -> [a -> Bool] -> a
suchThat' x ps | all (\p -> p x) ps = x

test :: [Int]
test = map (\p -> suchThat x p) [(> 2), (< 4)]
 where x free

suchThat'' :: a -> (a -> Bool) -> a
suchThat'' x p = suchThat' x [p]

(.:) :: a -> [a] -> [a]
(.:) = (:)

(.&) :: a -> [a]
(.&) = (:[])

suchThat :: a -> (a -> Bool) -> a
suchThat x p | p x = x

(|>) :: a -> Bool -> a
x |> b | b = x
