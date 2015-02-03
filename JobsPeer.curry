-- Jobs Puzzle
--
-- There are four people: Roberta, Thelma, Steve, and Pete.
-- Among them, they hold eight different jobs.
-- Each holds exactly two jobs.
-- The jobs are chef, guard, nurse, clerk, police officer (gender not implied),
-- teacher, actor, and boxer.
-- The job of nurse is held by a male.
-- The husband of the chef is the clerk.
-- Roberta is not a boxer.
-- Pete has no education past the ninth grade.
-- Roberta, the chef, and the police officer went golfing together.
--
-- Question: Who holds which jobs?

-- Comment: This program would be way more faster if one used `Success`
-- instead of `Bool`, but I don't care about performance for now.

data Job    = Chef    | Guard   | Nurse | Clerk
            | Officer | Teacher | Actor | Boxer
data Person = Roberta | Thelma  | Steve | Pete

allDifferent :: [a] -> Bool
allDifferent []     = True
allDifferent (x:xs) = x `notElem` xs && allDifferent xs

ordered :: (Job, Job) -> Bool
ordered (x, y) = x < y

pairs :: [Job] -> [(Job, Job)]
pairs []       = []
pairs [_]      = failed -- just to avoid a warning
pairs (x:y:ys) = (x, y) : pairs ys

jobs :: [(Person, (Job, Job))]
jobs | Actor           `elem`    males
    && Nurse           `elem`    males
    && Chef            `elem`    females
    && Clerk           `elem`    males
    && Boxer           `notElem` [r1, r2]
    && Nurse           `notElem` [p1, p2]
    && Officer         `notElem` [p1, p2]
    && Teacher         `notElem` [p1, p2]
    && Chef            `notElem` [r1, r2]
    && Officer         `notElem` [r1, r2]
    && (Chef, Officer) `notElem` jobPairs
    && allDifferent allJobs
    && all ordered  jobPairs
    =  zip [Roberta, Thelma, Steve, Pete] jobPairs
  where
  females@[r1, r2, _, _] = [_, _, _, _]
  males@[_, _, p1, p2]   = [_, _, _, _]
  jobPairs = pairs allJobs
  allJobs  = females ++ males
