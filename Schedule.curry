{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module Schedule where

import Combinators ((|>), allDifferent)

import Maybe (isJust)

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Bounded, Enum, Eq, Ord)

isWorkingDay :: Weekday -> Bool
isWorkingDay = not . isWeekendDay

isWeekendDay :: Weekday -> Bool
isWeekendDay = flip any weekend . (==)
-- weekendDay w = any (== w) [Saturday, Sunday]

weekend :: [Weekday]
weekend = [Saturday,Sunday]

type PeopleCount = Int
type DayCount = Int
type ShiftCount = Int

data Plan a b = Plan [(Weekday, [(a, b)])]

type Person a = { wplan :: [(Weekday, a)]}

workplan :: Person a -> [(Weekday,a)]
workplan p = p :> wplan

worksOn :: Person a -> Weekday -> Bool
p `worksOn` d = isJust (lookup d (workplan p))

assignedTo :: Eq a => Plan a b -> (Weekday, a) -> Maybe b
assignedTo (Plan plan) (d,shft) = maybe Nothing (lookup shft) (lookup d plan)

-- workingPlan :: Eq a => PeopleCount -> ShiftCount -> DayCount -> Plan a (Person a)
-- workingPlan ppl shfts ds = plan |> restrictions
--  where
--   restrictions =  allDifferent (concatMap workplan persons)
--                -- && not ((eachOf persons `worksOn`) `all` weekend)
--                && not (all (\p -> (p `worksOn`) `all` weekend) persons)
--                -- && 
--   persons = genVars ppl
--   -- days    = take ds (concat (repeat [minBound..maxBound]))
--   shifts  = genVars shfts
--   plan free


genVars :: Int -> [a]
genVars n = if n==0 then [] else _ : genVars (n-1)

p1 = { wplan := [(Monday, 1), (Tuesday, 2), (Saturday,3)] }
p2 = { wplan := [(Saturday,1), (Sunday, 3)] }

test1 :: ([Int],[Weekday])
test1 = zip (replicate 7 [1..4]) [minBound .. maxBound]