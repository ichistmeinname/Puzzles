{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module ExperimentalPuzzle where

import SetFunctions
import Binary

type Set a = [a]

insert :: Eq a => a -> Set a -> Set a
insert x ys | x `elem` ys = ys
            | otherwise   = x:ys

mapSet :: Eq b => (a -> b) -> Set a -> Set b
mapSet f []       = []
mapSet f ys@(_:_) = mapHelp ys []
 where
  mapHelp []     xs = xs
  mapHelp (z:zs) xs = insert (f z) xs

listToSet :: Eq a => [a] -> Set a
listToSet = foldr insert []

card :: Set a -> BinInt
card = lengthIt . foldr consIt Empty

is :: BinInt -> Int -> Bool
is x y = x == (intToBinInt y)

intToBinInt n = case n of
                     0 -> Zero
                     _ | n > 0 -> Pos (intToBinInt' n)
intToBinInt' n = case n of
                      1 -> IHi
                      _ -> Binary.succ (intToBinInt' (n - 1))

elemOf :: Eq a => a -> Set a -> Bool
elemOf = elem

data Person = Person Name Gender
  deriving (Eq,Ord,Show)
data Name = Pete | Roberta | Steve | Thelma
  deriving (Eq,Ord,Show)
data Gender = Male | Female
  deriving (Eq,Ord,Show)

people :: Set Name
people = listToSet [Roberta,Thelma,Steve,Pete]

-- female :: Set Name
-- female = listToSet [Thelma, Roberta]

-- male :: Set Name
-- male = listToSet [Steve,Pete]

jobs :: Set JobName
jobs = [Actor,Boxer,Chef,Clerk,Guard,Nurse,Police,Teacher]

type Surj a b = a -> b

holdsJobSur :: Surj JobName Name
holdsJobSur j = y where y free
 -- map genFuncs jobs
 -- where
 --   genFuncs job = let y = unknown
 --                  in \job -> y 

equation |  holdsJobSur Nurse `elemOf` male
         && holdsJobSur Actor `elemOf` male
         && husband (holdsJobSur Chef) male female == holdsJobSur Clerk
         && not (Boxer `elemOf` jobsOf Roberta)
         && not (Pete `elemOf` mapSet holdsJobSur qualifiedJobs)
         && card golfers `is` 3
         = map (\p -> let js = jobsOf p in cond ((card js `is` 2) =:= True) (p, js)) people
 where
  female :: Set Name
  female = listToSet [Thelma, Roberta]
  male :: Set Name
  male = listToSet [Steve,Pete]
  golfers :: Set Name
  golfers = listToSet [Roberta, holdsJobSur Chef, holdsJobSur Police]
jobsOf :: Name -> Set JobName
jobsOf = (holdsJobSur ~~.)

(~~.) :: (Eq a, Eq b) => Surj a b -> b -> Set a
f ~~. x | allOfSet (\y -> f y == x) ys = ys
 where
  allOfSet :: (a -> Bool) -> Set a -> Bool
  allOfSet f []     = True
  allOfSet f (x:xs) = f x && allOfSet f xs
  ys = z `insert` zs
  z, zs free
       
husband :: Name -> Set Name -> Set Name -> Name
husband x male female | x `elemOf` male && y `elemOf` female = y where y free

qualifiedJobs :: Set JobName
qualifiedJobs = listToSet [Police,Teacher,Nurse]

-- cond ((holdsJobSur Nurse `elemOf` male) =:= True & (holdsJobSur Actor `elemOf` male) =:= True & (husband (holdsJobSur Chef) == holdsJobSur Clerk) =:= True & (not (Boxer `elemOf` jobsOf Roberta)) =:= True & (not (Pete `elemOf` map holdsJobSur qualifiedJobs)) =:= True & (card golfers == 3) =:= True) (map (\p -> let js = jobsOf p in cond ((card js == 2) =:= True) (p, js)) people)

data Job = Job JobName Gender
  deriving (Eq,Ord,Show)
data JobName = Actor | Boxer | Chef | Clerk | Guard | Nurse | Police | Teacher
  deriving (Eq, Ord, Show)

-- inverse function
(:~) :: Eq b => (a -> b) -> b -> a
(:~) f y | f x == y = x where x free

-- (~.) :: [a] -> (a -> b) -> [b]
-- (~.) = flip ($)

(~.~) :: a -> b -> (a,b)
x ~.~ y = (x,y)

data Iterator a = Next (Iterate a) | Empty
data Iterate a = It a | II (Iterate (a,a)) a | OI (Iterate (a,a))

consIt :: a -> Iterator a -> Iterator a
consIt x' xs' = Next (cons x' xs')
 where
  cons x Empty         = It x
  cons x (Next is) = cons' x is
  cons' :: a -> Iterate a -> Iterate a
  cons' x (It i)    = OI (It (x,i))
  cons' x (OI is)   = II is x
  cons' x (II is i) = OI (cons' (x,i) is)

-- Functor (Iterator a)
(^.) :: Iterator a -> (a -> b) -> Iterator b
Empty ^. f = Empty
Next i ^. f = Next (iMap f i)
 where
  iMap :: (a -> b) -> Iterate a -> Iterate b
  iMap f (It x)   = It (f x)
  iMap f (II i x) = II (iMap (\ (y,z) -> (f y, f z)) i) (f x)
  iMap f (OI i)   = OI (iMap (\ (y,z) -> (f y, f z)) i)

lengthIt :: Iterator a -> BinInt
lengthIt Empty           = Zero
lengthIt (Next list) = Pos (lengthI list)
 where
  lengthI :: Iterate a -> Nat
  lengthI (It _)   = IHi
  lengthI (OI l)   = O (lengthI l)
  lengthI (II l _) = I (lengthI l)