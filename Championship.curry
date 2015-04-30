{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module Championship where

import Combinators

import Float ((/.))
import List ((\\))
import Maybe (fromJust)

import SetFunctions (foldValues, isEmpty, mapValues, set0, set2, set3, Values)

data Team = Frankfurt | Gladbach | Dortmund | Paderborn | Leverkusen | Hannover
       | Hoffenheim | Muenchen | Hertha | Koeln | Freiburg | Mainz | Augsburg
       | Stuttgart | Bremen | HamburgerSV | Wolfsburg | Schalke
  deriving (Eq,Show)

data Match = Match Team Team Result
  deriving (Eq,Show)
data Result = AwayVictory | Draw | HomeVictory
  deriving (Bounded,Enum,Eq,Show)

possibleResults :: [Result]
possibleResults = [minBound .. maxBound]

-- data Score = Int ::: Int

weakerThan :: (Team,Int) -> (Team,Int) -> Bool
weakerThan (_,n) (_,m) = n < m

points :: Result -> (Int,Int)
points HomeVictory = (3,0)
points Draw        = (1,1)
points AwayVictory = (0,3)

matchPoints :: Match -> ((Team, Int),(Team, Int))
matchPoints (Match t1 t2 res) = ((t1,res1),(t2,res2))
 where
  (res1,res2) = points res

type Table = [TableEntry]
type TableEntry = (Team,Int)

type Matchday = [MatchdayEntry]
type MatchdayEntry = (Team,Team)

lookup_ :: Eq a => a -> [(a,b)] -> b
lookup_ x = fromJust . lookup x

update :: Eq a => a -> (b -> b) -> [(a,b)] -> [(a,b)]
update _ _ [] = []
update key f ((k,v):kvMap)
  | k == key  = (k,f v) :kvMap
  | otherwise = (k,v) : update key f kvMap

addPoints :: Team -> Int -> Table -> Table
addPoints k v = update k (+ v)

recalculateTable :: Match -> Table -> Table
recalculateTable (Match team1 team2 result) table =
  addPoints team1 res1 (addPoints team2 res2 table)
 where
  (res1,res2) = points result

updateTable :: [Matchday] -> Table -> (Table,[Match])
updateTable mds table = (foldr recalculateTable table matches,matches)
 where
  matches = concatMap playMatchDay mds

match :: Team -> Team -> Match
match t1 t2 = Match t1 t2 _

playMatchDay :: Matchday -> [Match]
playMatchDay = map (uncurry match)

day31 = [ (Schalke,Stuttgart), (Wolfsburg,Hannover),
          (Freiburg,Paderborn),(Mainz,HamburgerSV)]
table30 = [(Freiburg,30),(Hannover, 29),
           (HamburgerSV,28), (Paderborn, 28),(Stuttgart, 27)]

matchDay31 :: Matchday
matchDay31 =  [ (Schalke,Stuttgart), (Wolfsburg,Hannover), (Augsburg,Koeln)
              , (Hoffenheim,Dortmund), (Bremen,Frankfurt), (Freiburg,Paderborn)
              , (Leverkusen,Muenchen), (Mainz,HamburgerSV), (Hertha,Gladbach) ]

matchDay32 :: Matchday
matchDay32 = [ (HamburgerSV,Freiburg), (Muenchen,Augsburg), (Dortmund,Hertha)
             , (Gladbach,Leverkusen), (Hannover,Bremen), (Frankfurt,Hoffenheim)
             , (Stuttgart,Mainz), (Paderborn,Wolfsburg), (Koeln,Schalke) ]

matchDay33 :: Matchday
matchDay33 = [ (Schalke,Paderborn), (Leverkusen,Hoffenheim), (Stuttgart,HamburgerSV)
             , (Wolfsburg,Dortmund), (Mainz,Koeln), (Augsburg,Hannover)
             , (Hertha,Frankfurt), (Bremen,Gladbach), (Freiburg,Muenchen) ]

matchDay34 :: Matchday
matchDay34 = [ (Muenchen,Mainz), (Dortmund,Bremen), (Gladbach,Augsburg)
             , (Hoffenheim,Hertha), (Hannover,Freiburg), (Frankfurt,Leverkusen)
             , (HamburgerSV,Schalke), (Koeln,Wolfsburg), (Paderborn,Stuttgart) ]

upcomingMatchdays :: [Matchday]
upcomingMatchdays =
  [matchDay31,matchDay32,matchDay33,matchDay34]

currentTable :: Table
currentTable =
  [(Muenchen, 76), (Wolfsburg, 61), (Gladbach, 57), (Leverkusen, 55)
  ,(Schalke, 42), (Augsburg, 42), (Hoffenheim, 40), (Dortmund,39)
  ,(Bremen, 39), (Mainz, 37),(Frankfurt, 36), (Koeln, 35), (Hertha, 34)
  ,(Freiburg, 30), (Hannover, 29), (HamburgerSV,28), (Paderborn, 28)
  ,(Stuttgart, 27)]

type Question a = Team -> [Matchday] -> Table -> a

relegation :: Team
           -> [Matchday]
           -> Table
           -> ((Table,[Match]),([Matchday],Table))
relegation team mds curTable =
  (relegation' team matchDays table, (matchDays,table))
 where
  matchDays = filterMatchdays teams mds
  table     = filter ((< pointsBound) . snd) curTable
  teams     = map fst table
  pointsBound = currentPoints team curTable + maxPoints mds

champion :: Team
         -> [Matchday]
         -> Table
         -> ((Table,[Match]),([Matchday],Table))
champion team mds curTable =
  (champion' team matchDays table,(matchDays,table))
 where
  matchDays = filterMatchdays teams mds
  table     = filter ((> pointsBound) . snd) curTable
  teams     = map fst table
  pointsBound = currentPoints team curTable - maxPoints mds

test i = percentageForQuestion relegation HamburgerSV (take i upcomingMatchdays) currentTable

filterMatchdays :: [Team] -> [Matchday] -> [Matchday]
filterMatchdays teams matchDays =
    map (filter (\ (t1,t2) ->  any (`elem` teams)
                                   [t1,t2]))
        matchDays

relegation' :: Team
            -> [Matchday]
            -> Table
            -> (Table,[Match])
relegation' team matchDays tableExcerpt =
  r
    |> not (isEmpty (thereExistN 2 (newTable `without` team)
       `suchThat` anySet (all (`weakerThan` teamEntry))))
       -- `suchThatSet` all (`weakerThan` teamEntry)))
 where
  teamEntry = (team, currentPoints team newTable)
  r@(newTable,results) = updateTable matchDays tableExcerpt

champion' :: Team
          -> [Matchday]
          -> Table
          -> (Table,[Match])
champion' team matchDays tableExcerpt =
  r
    |> not (isEmpty (set0 (newTable `without` team
       `suchThat` all (`weakerThan` teamEntry))))
 where
  teamEntry = (team, currentPoints team newTable)
  r@(newTable,results) = updateTable matchDays tableExcerpt

without :: Table -> Team -> Table
without []         _ = []
without (e@(t,_):ts) team
  | t == team = ts
  | otherwise = e : without ts team

maxPoints :: [Matchday] -> Int
maxPoints mds = fst (points (maxBound :: Result)) * length mds

currentPoints :: Team -> Table -> Int
currentPoints = lookup_

isPossible :: Question a -> Team -> [Matchday] -> Table -> Bool
isPossible q team mds table = not (isEmpty (set3 q team mds table))

percentageForQuestion :: Question (a,([Matchday],Table))
                      -> Team
                      -> [Matchday]
                      -> Table
                      -> (Float,Int,Int,[Matchday])
percentageForQuestion q team mds curTable =
  ((fromInteger pos1 / fromInteger pos2) * 100,pos1,pos2, matches)
 where
  pos1    = countValues (set3 q team matches table)
  pos2    = countOutcomes matches
  (matches,table) = snd (q team mds curTable)

countValues :: Values a -> Int
countValues = foldValues (\_ n -> n + 1) 0 . mapValues (\_ -> 1)

countOutcomes :: [Matchday] -> Int
countOutcomes mds = length possibleResults `pow` length (concat mds)

allSet :: (a -> Bool) -> Values a -> Bool
allSet p = foldValues (&&) True . mapValues p

anySet :: (a -> Bool) -> Values a -> Bool
anySet p = foldValues (||) False . mapValues p

suchThatSet :: Values a -> (a -> Bool) -> Values a
suchThatSet vs p | foldValues (||) False (mapValues p vs) = vs

thereExist :: Eq a => Int -> [a] -> [a]
thereExist = nOf_

thereExistN :: Eq a => Int -> [a] -> Values [a]
thereExistN n xs = set2 nOf_ n xs -- nOf n xs `suchThat` allDifferent

nOf_ :: Int -> [a] -> [a]
nOf_ 0 [] = []
nOf_ n (x:xs) | n == 0    = []
              | otherwise = x : nOf_ (n-1) xs ? nOf_ n xs

pow :: Integral a => a -> a -> a
pow a b | b>= 0 = powaux 1 a b
  where
    powaux n x y = if y == 0 then n
                   else powaux (n * if (y `mod` 2 == 1) then x else 1)
                               (x * x)
                               (y `div` 2)