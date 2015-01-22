{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

import SetFunctions
import List (nub,sortBy)

data Person = Person Name Gender
  deriving (Eq,Show)
data Name = Roberta | Thelma | Steve | Pete
  deriving (Eq,Show)
data Gender = Male | Female
  deriving (Eq,Show)

isFemale :: Person -> Bool
isFemale (Person _ Female) = True
isFemale (Person _ Male)   = False

isMale :: Person -> Bool
isMale = not . isFemale

data Job = Job JobName Gender
  deriving (Eq,Show)
data JobName = Actor | Boxer | Chef | Clerk | Guard | Nurse | Police | Teacher
  deriving (Eq, Ord, Show)

-- property1 :: Job -> Person
-- property1 (Job _ g) = (Person _ g)
    
husband :: Person -> Person
husband (Person _ Female) = (Person _ Male)

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
data Iterate a = It a | I (Iterate (a,a)) a | O (Iterate (a,a))

eachOf :: [a] -> Iterator a
eachOf = foldr consIt Empty
 where

consIt :: a -> Iterator a -> Iterator a
consIt x' xs' = Next (cons x' xs')
 where
  cons x Empty         = It x
  cons x (Next is) = cons' x is
  cons' :: a -> Iterate a -> Iterate a
  cons' x (It i)   = O (It (x,i))
  cons' x (O is)   = I is x
  cons' x (I is i) = O (cons' (x,i) is)

-- Functor (Iterator a)
(^.) :: Iterator a -> (a -> b) -> Iterator b
Empty ^. f = Empty
Next i ^. f = Next (iMap f i)
 where
  iMap :: (a -> b) -> Iterate a -> Iterate b
  iMap f (It x)  = It (f x)
  iMap f (I i x) = I (iMap (\ (y,z) -> (f y, f z)) i) (f x)
  iMap f (O i)   = O (iMap (\ (y,z) -> (f y, f z)) i)

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