{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

import SetFunctions
import List (nub,sortBy)
import Binary

type Set a = [a]

insert :: Eq a => a -> Set a -> Set a
insert x ys | x `elem` ys = ys
            | otherwise   = x:ys

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
  deriving (Eq,Show)
data Name = Roberta | Thelma | Steve | Pete
  deriving (Eq,Show)
data Gender = Male | Female
  deriving (Eq,Show)

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
         && husband (holdsJobSur Chef) == holdsJobSur Clerk
         && not (Boxer `elemOf` jobsOf Roberta)
         && not (Pete `elemOf` map holdsJobSur qualifiedJobs)
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
  y, z, zs free
       
husband :: Name -> Name
husband x | x `elemOf` male && y `elemOf` female = y
 where y free

qualifiedJobs :: Set JobName
qualifiedJobs = listToSet [Police,Teacher,Nurse]
  p free

-- cond ((holdsJobSur Nurse `elemOf` male) =:= True & (holdsJobSur Actor `elemOf` male) =:= True & (husband (holdsJobSur Chef) == holdsJobSur Clerk) =:= True & (not (Boxer `elemOf` jobsOf Roberta)) =:= True & (not (Pete `elemOf` map holdsJobSur qualifiedJobs)) =:= True & (card golfers == 3) =:= True) (map (\p -> let js = jobsOf p in cond ((card js == 2) =:= True) (p, js)) people)

data Job = Job JobName Gender
  deriving (Eq,Show)
data JobName = Actor | Boxer | Chef | Clerk | Guard | Nurse | Police | Teacher
  deriving (Eq, Ord, Show)

-- property1 :: Job -> Person
-- property1 (Job _ g) = (Person _ g)
    
-- husband :: Person -> Person
-- husband (Person _ Female) = (Person _ Male)

pete, steve, thelma, roberta :: Person
pete = Person Pete Male
steve = Person Steve Male
thelma = Person Thelma Female
roberta = Person Roberta Female

persons = [pete,steve,thelma,roberta]

isQualifiedJob :: JobName -> Bool
isQualifiedJob = (`elem` [Police, Teacher, Nurse])

jobCount :: [Job] -> Success
jobCount jobs = length jobs =:= 8

-- data MyPersons = MP Person Person Person Person

-- myPersons :: Person -> Person -> Person -> Person -> MyPersons
-- myPersons p1 p2 p3 p4 | all (`elem` persons) ps && allDifferent ps = MP p1 p2 p3 p4
--  where
--   ps = [p1,p2,p3,p4]

holdsJob :: Job -> Person
-- holdsJob (Job j Male)   | not (isQualifiedJob j) = pete
holdsJob (Job Boxer g)  | not (p == Roberta)     = Person p g where p free
holdsJob (Job Chef g)   | g == Female
                        && not (p == Roberta)    = Person p g where p free
holdsJob (Job Nurse g)  | g == Male              = Person _ g
holdsJob (Job Actor g)  | g == Male              = Person _ g
holdsJob (Job Clerk g)  | g == Male              = Person _ g
holdsJob (Job Teacher g)                         = Person _ g
holdsJob (Job Guard g)                           = Person _ g
holdsJob (Job Police g) | not (p == Roberta)     = Person p g where p free

jobSituationOf :: Person -> (Person,Job,Job)
jobSituationOf p@(Person name g) |  j1 /= j2 && j1 <= j2
                                 && not (j1 == Chef && j2 == Police)
                                 && not (name == Pete && (isQualifiedJob j1 || isQualifiedJob j2))
                                 && holdsJob (Job j1 g) == p
                                 && holdsJob (Job j2 g) == p = (Person name g, Job j1 g, Job j2 g)
   where j1,j2 free

solve :: [(Person,Job,Job)]
solve |  allDifferentJobs js
      -- && all (`elem` persons) ps
      -- && allDifferent ps
      = sortBy (==) js
 where js = map jobSituationOf persons
       -- ps = [_,_,_,_]

allDifferent :: Eq a => [a] -> Bool
allDifferent xs = not (or [ x == y | (x,id1) <- ys, (y,id2) <- ys, id1 /= id2])
 where ys = zip xs [0..]

allDifferentJobs :: [(Person,Job,Job)] -> Bool
allDifferentJobs = allDifferent . foldr (\ (_,(Job j1 _),(Job j2 _)) jAll -> j1:j2:jAll) []


-- inverse function
(:~) :: Eq b => (a -> b) -> b -> a
(:~) f y | f x == y = x where x free

(~.) :: a -> (a -> b) -> b
(~.) = flip ($)

-- twoJobs :: Person -> (Job,Job)
twoJobs = each ~. hasJob `ofSize` 2

twoJobs' = eachOf persons ^. hasJob `ofSize` 2

data Iterator a = Next (Iterate a) | Empty
data Iterate a = It a | II (Iterate (a,a)) a | OI (Iterate (a,a))

eachOf :: [a] -> Iterator a
eachOf = foldr consIt Empty
 where

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
  lengthI (It _) = IHi
  lengthI (OI l)   = O (lengthI l)
  lengthI (II l _) = I (lengthI l)

data JobPosition = P Job Job

hasJob :: Person -> Job
hasJob = unknown
-- hasJob p | oneof ~. == p = j1

class OneOf a where
  oneOf :: a -> a

instance OneOf Job where
  oneOf j@(Job Clerk _) = j

ofSize :: a -> Int -> Bool
ofSize val size = unknown

class Countable a where
  ofSize' :: a -> Int

instance Countable (Person -> Job) where
  ofSize' = unknown

class Eachable a where
  each :: a

instance Eachable Person where
  each = pete
  each = roberta
  each = thelma
  each = steve