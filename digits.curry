{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}
 
import Constraint ( andC )
 
validDigits :: [Int]
validDigits = [0 .. 9]
 
solveWith :: [Int] -> [Int]
solveWith vds | zipWith numberOfIn vds (repeat digits) =:= digits = digits where
    digits = [unknown' | _ <- vds]
 
unknown' :: Int
unknown' = foldr1 (?) [9,8,7,6,5,4,3,2,1]
 
solve :: [Int]
solve = solveWith validDigits
 
numberOfIn :: Eq a => a -> [a] -> Int
numberOfIn x = length . filter (== x)
 
sumOpThan :: Num a => (a -> a -> Bool) -> [a] -> a -> Success
sumOpThan op xs n = go 0 xs where
    go k (x : xs) | k `op` n  = go (k + x) xs
    go _ []                   = success

-- let digits = foldr1 (?) [9,8,7,6,5,4,3,2,1] in digits =:= zipWith numberOfIn (enumFromTo 0 9) (repeat digits) where a0,a1,a2,a3,a4,a5,a6,a7,a8,a9 free