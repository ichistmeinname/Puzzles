{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module Puzzle where

import Combinators

import List (intercalate,isInfixOf)
import SetFunctions (set0,values2list)

data Person = Person Name Gender [Job]
  deriving (Eq,Ord)

instance Show Person where
  show (Person name gender js) =
    show name ++ paren (show gender) ++ ": " ++ intercalate ", " (map show js)

person :: Name -> Gender -> Job -> Job -> Person
person n g j1 j2 | all ((==) g . gender) js && j1 < j2 = Person n g js
 where
  js = [j1,j2]

jobs :: Person -> [Job]
jobs (Person _ _ js) = js

jobDesc :: Person -> Job
jobDesc p@(Person _ g _) | p `hasJob` j = Job j g
 where j free

job :: Person -> JobName
job p@(Person _ g js) = jobName (oneOf js)

hasJob :: Person -> JobName -> Bool
hasJob (Person _ g jobs) job = any ((== job) . jobName) jobs 

data Gender = Male | Female
  deriving (Eq,Ord,Show)

data Name = Pete | Roberta | Steve | Thelma
  deriving (Eq,Ord,Show)

data Job = Job JobName Gender
  deriving (Eq,Ord)

instance Show Job where
  show (Job name gender) = show name ++ paren (show gender)

paren :: String -> String
paren str = " (" ++ str ++ ")"

data JobName = Actor | Boxer | Chef | Clerk | Guard | Nurse | Police | Teacher
  deriving (Eq,Ord,Show)

jobName :: Job -> JobName
jobName (Job name _) = name

class HasGender a where
  gender :: a -> Gender

instance HasGender Person where
  gender (Person _ g _) = g

instance HasGender Job where
  gender (Job _ g) = g

twoJobs :: [Person]
twoJobs = persons |> -- (persons `have` different) job
                     allDifferentF jobName (concatMap jobs persons)
                  |> (roberta `hasNot` job) Boxer
                  |> (pete `hasNot` job) `with` higherEducation
                  -- |> allDifferent [ roberta
                  --    , oneOf persons `suchThat` (`hasJob` Chef)
                  --    , oneOf persons `suchThat` (`hasJob` Police)]
                  |> ([ roberta
                     , oneOf persons `suchThat` (`hasJob` Chef)
                     , oneOf persons `suchThat` (`hasJob` Police)] `have_` different) id
                  |> p2 == husband (p1 `suchThat` (`hasJob` Chef))
                  |> (p2 `has` job) Clerk
                  |> ((oneOf persons `suchThat` ((`has` job) `asA` Nurse)) `has`) gender Male
                  |> ((oneOf persons `suchThat` ((`has` job) `asAn` Actor)) `has`) gender Male
 where
  p1 = oneOf persons
  persons = [pete,roberta,steve,thelma]
  pete = person Pete Male _ _
  roberta = person Roberta Female _ _
  steve = person Steve Male _ _
  thelma = person Thelma Female _ _
  p2 = oneOf persons
  higherEducation = [Teacher,Nurse,Police]

husband :: Person -> Person
husband (Person _ Female _) | gender p_ == Male = p_
 where p_ free

-- solution :: IO ()
solution = values2list (set0 twoJobs) >>= putStr . intercalate "\n" . map (unlines . map show)

-- persons' = [pete',roberta',steve',thelma']
-- pete' = person Pete Male _ _
-- roberta' = person Roberta Female _ _
-- steve' = person Steve Male _ _
-- thelma' = person Thelma Female _ _


-- |> roberta) jobName (isNot Boxer)
--             |> pete) jobName (\x -> all (isNot x) [Teacher,Nurse,Police])
--          |> _) jobName (\x ->
--  where
--   isNot = (/=)

-- size :: [a] -> Int -> Bool
-- size xs n = help xs (replicate n _)
--  where
--   help :: [a] -> [a] -> Bool
--   help []     []     = True
--   help (_:xs) (_:ys) = help xs ys

-- with :: (a, a -> b) -> (b -> c -> Bool) -> c -> Bool
-- with (x, f) p v = p (f x) v

-- withL :: ([a],a -> b) -> (b -> c -> Bool) -> c -> Bool
-- withL (xs,f) p v = all (\x -> with (x,f) p v)  xs


-- has :: a -> (a -> b) -> b
-- has = flip ($)

-- hasL :: [a] -> (a -> b) -> [b]
-- hasL = flip map


-- (|>) :: Eq a => [(a,[b])] -> a -> (b -> c) -> (c -> Bool) -> [(a,[b])]
-- (|>) xs filterVal selF condF =
--   filter (\ (x,ys) -> (x == filterVal && all (condF . selF) ys) || x /= filterVal) xs


-- ofSize' :: Eq b => (a, a -> b) -> Int -> [b]
-- ofSize' (x,f) i = case i of
--                        0 -> []
--                        _ -> f x : ofSize' (x,f) (i - 1)

-- ofSize :: Eq b => ([a], a -> b) -> Int -> [(a,[b])]
-- ofSize ([]  ,_)     i = []
-- ofSize (x:xs,f)     i = (x,ofSize' (x,f) i) : ofSize (xs,f) i

-- class OneOf a where
--   oneOf :: a

-- instance OneOf Job where
--   oneOf = case Job _ _ of
--                Job Actor Female -> failed
--                j                -> j


-- class Ord a => Eachable a where
--   each :: a
--   each = _
--   every :: [a]
--   every = sortValues (set0 each)

-- instance Eachable Person where
-- --   each = person Pete Male [_,_]
-- --   each = person Roberta Female [_,_]
-- --   each = person Steve Male [_,_]
-- --   each = person Thelma Female [_,_]

-- instance Eachable Job where
--   each = case Job _ _ of
--               -- Job Actor Female -> failed
--               -- Job Nurse Female   -> failed
--               j                -> j

-- instance Eachable Gender where
--   -- each = Female
--   -- each = Male

-- instance Eachable JobName where
--   -- each = Clerk
--   -- each = Actor
--   -- each = Teacher
--   -- each = Guard
--   -- each = Chef
--   -- each = Boxer
--   -- each = Police
--   -- each = Nurse

-- -- inverse
-- (~.) :: Eq b => (a -> b) -> b -> a
-- f ~. y | f x == y = x
--  where x free

-- (~.~) :: a -> b -> (a,b)
-- x ~.~ y = (x,y)