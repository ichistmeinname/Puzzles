{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module NaivePuzzle where

import SetFunctions (set0,values2list)
import List         (intercalate)

{-
There are four people: Roberta, Thelma, Steve, and Pete. 
Among them, they hold eight different jobs. 
Each holds exactly two jobs. 
The jobs are chef, guard, nurse, clerk, police officer (gender not implied), teacher, actor, and boxer. 
The job of nurse is held by a male. 
The husband of the chef is the clerk. 
Roberta is not a boxer. 
Pete has no education past the ninth grade. 
Roberta, the chef, and the police officer went golfing together.
-}

data Person = Person Name Gender
  deriving (Eq,Ord,Show)
data Name = Pete | Roberta | Steve | Thelma
  deriving (Eq,Ord,Show)
data Gender = Male | Female
  deriving (Eq,Ord,Show)

data Job = Job JobName Gender
  deriving (Eq,Ord,Show)
data JobName = Actor | Boxer | Chef | Clerk | Guard | Nurse | Police | Teacher
  deriving (Eq, Ord, Show)

holdsJob :: Job -> Person
holdsJob (Job Chef g)    |  g == Female
                         && not (n == Roberta) = Person n g where n free
holdsJob (Job Police g)  |  not (n == Roberta)
                         && not (n == Pete)    = Person n g where n free
holdsJob (Job Nurse g)   |  g == Male
                         && not (n == Pete)    = Person n g where n free
holdsJob (Job Teacher g) | not (n == Pete)     = Person n g where n free
holdsJob (Job Boxer g)   | not (n == Roberta)  = Person n g where n free
holdsJob (Job Actor g)   | g == Male           = Person _ g
holdsJob (Job Clerk g)   | g == Male           = Person _ g
holdsJob (Job Guard g)                         = Person _ g
-- holdsJob (Job j Male)   | not (isQualifiedJob j) = pete

jobSituationOf :: Person -> (Person,Job,Job)
jobSituationOf p@(Person name g)
  |  j1 /= j2 && j1 <= j2
  && not (j1 == Chef && j2 == Police)
  -- && not (name == Pete && (isQualifiedJob j1 || isQualifiedJob j2))
  && holdsJob (Job j1 g) == p
  && holdsJob (Job j2 g) == p = (Person name g, Job j1 g, Job j2 g)
 where j1,j2 free

quiz | allDifferentJobs js = js
 where
  js = map jobSituationOf [pete,steve,thelma,roberta]

pete, steve, thelma, roberta :: Person
pete = Person Pete Male
steve = Person Steve Male
thelma = Person Thelma Female
roberta = Person Roberta Female

-----
allDifferent :: Eq a => [a] -> Bool
allDifferent [] = True
allDifferent (x:xs) = all (/=x) xs && allDifferent xs

allDifferentJobs :: [(Person,Job,Job)] -> Bool
allDifferentJobs = allDifferent . foldr (\ (_,(Job j1 _),(Job j2 _)) jAll -> j1:j2:jAll) []

-- isQualifiedJob :: JobName -> Bool
-- isQualifiedJob = (`elem` [Police, Teacher, Nurse])

-----
solve = values2list (set0 quiz) >>= putStr . intercalate "\n" . map (unlines . map show)