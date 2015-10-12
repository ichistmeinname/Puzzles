{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

import FiniteMap ( lookupFM, listToFM, FM )
import List      ( sort )
import Maybe     ( fromMaybe )

import SetFunctions ( set0, values2list )

-- Data structure for relations over finite domains.

data Rel a b = Rel (FM (a, b) Bool) [a] [b]

-- Returns the domain of the argument relation.

firstValues :: Rel a b -> [a]
firstValues (Rel _ as _) = as

-- Returns the codomain of the argument relation.

secondValues :: Rel a b -> [b]
secondValues (Rel _ _ bs) = bs

-- Given a relation and a row "name" this function returns the corresponding row.
-- The computations forces a choice for non-determnistic values.

row :: (Ord a, Ord b) => Rel a b -> a -> [b]
row (Rel fm _ bs) a = [b | b <- bs, fm ! (a, b)]

-- Given a relation and a column "name" this function returns the corresponding column.
-- The computations forces a choice for non-determnistic values.

col :: (Ord a, Ord b) => Rel a b -> b -> [a]
col (Rel fm as _) b = [a | a <- as, fm ! (a, b)]

-- Checks whether all rows of a relation satisfy a given predicate.

allRows :: (Ord a, Ord b) => ([b] -> Bool) -> Rel a b -> Bool
allRows p r = all (p . row r) (firstValues r)

-- Checks whether all columns of a relation satisfy a given predicate.

allColumns :: (Ord a, Ord b) => ([a] -> Bool) -> Rel a b -> Bool
allColumns p r = all (p . col r) (secondValues r)

-- Given a FiniteMap and a key, this function looks up the value of the key. If the key is present,
-- the value of the key is returned, otherwise the computation fails.

(!) :: Ord k => FM k v -> k -> v
fm ! k = fromMaybe failed (lookupFM fm k)

-- "Pretty"-prints a relation.

showRel :: (Ord a, Ord b, Show a, Show b) => Rel a b -> String
showRel r = unlines [unwords [show a, ":", show (row r a)] | a <- firstValues r]

-- Begin: Auxiliaries -------------------------------------------------------------------------------

-- Lists all values of a bounded, enumerable data type.

listAll :: (Enum a, Bounded a) => [a]
listAll = [minBound .. maxBound]

-- Checks whether a list has a given size. This check is sufficiently lazy an works on infinite lists.

hasLength :: [a] -> Int -> Bool
hasLength []       n = n == 0
hasLength (_ : xs) n = n > 0 && hasLength xs (n - 1)

-- Given two ranges an some filled positions, this function creates a relation whose missing
-- positions are filled with free variables.

mkNonDetRel :: (Ord a, Ord b) => [a] -> [b] -> [((a, b), Bool)] -> Rel a b
mkNonDetRel as bs filled = Rel (listToFM (<) [((a, b), f a b) | a <- as, b <- bs]) as bs where
    f a b = fromMaybe (let x free in x) (lookup (a, b) filled)

toRelational :: [(a, b, Bool)] -> [((a, b), Bool)]
toRelational = map (\(x, y, b) -> ((x, y), b))

-- End : Auxiliaries --------------------------------------------------------------------------------

-- Begin: Jobs-Puzzle

-- Data type for the jobs.

data Job  = Chef | Guard | Nurse | Clerk | PoliceOfficer | Teacher | Actor | Boxer
    deriving (Eq, Ord, Enum, Bounded, Show)

-- Data type for the people.

data Name = Roberta | Thelma | Steve | Pete
    deriving (Eq, Ord, Enum, Bounded, Show)

-- The list of all possible jobs.

jobs :: [Job]
jobs = listAll

-- The list of all possible people.

names :: [Name]
names = listAll

-- The actual solution is a relation that has exactly one value in every column and exactly two
-- values in every row. Additionally, some properties are simpler to express in terms of rows,
-- rather than a list of negations.

solution :: Rel Name Job -> Rel Name Job
solution r | allRows rowPred r && allColumns colPred r = r where
    rowPred row = hasLength row 2 
                  && 
                  sort row /= sort [Chef, Clerk] 
                  && 
                  sort row /= sort [Chef, PoliceOfficer]
    
    colPred col = hasLength col 1

-- Jobs Puzzle
-- 
-- There are four people: Roberta, Thelma, Steve, and Pete. 
-- Among them, they hold eight different jobs. 
-- Each holds exactly two jobs. 
-- The jobs are chef, guard, nurse, clerk, police officer (gender not implied), teacher, actor, and boxer. 
-- The job of nurse is held by a male. 
-- The husband of the chef is the clerk. 
-- Roberta is not a boxer. 
-- Pete has no education past the ninth grade. 
-- Roberta, the chef, and the police officer went golfing together. 
-- 
-- Question: Who holds which jobs?
-- (Quelle: [1])

-- The preconditions specified by the puzzle.

preconditions :: Rel Name Job
preconditions = mkNonDetRel names jobs . toRelational $
  [ (Thelma, Nurse, False), (Roberta, Nurse, False)         -- nurse is not female
  , (Roberta, Clerk, False), (Thelma, Clerk, False)         -- Husband is clerk ==> clerk not female
  , (Roberta, Boxer, False)                                 -- Roberta is not the boxer
  , (Roberta, Chef, False), (Roberta, PoliceOfficer, False) -- Roberta is neither chef, nor police officer
  , (Pete, Nurse, False), (Pete, Teacher, False)            -- Pete is not the sharpest tool in the shed
     , (Pete, PoliceOfficer, False)                         -- Meta: Smarts required by Teacher, Nurse, PoliceOfficer
  , (Thelma, Actor, False), (Roberta, Actor, False)         -- Meta: actor male
  , (Pete, Chef, False), (Steve, Chef, False)               -- Meta: heterosexual marriage
    ]

solve :: IO ()
solve = values2list (set0 (solution preconditions)) >>= mapIO_ (putStrLn . showRel)
