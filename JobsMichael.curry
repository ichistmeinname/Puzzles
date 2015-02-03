{-
Jobs Puzzle

There are four people: Roberta, Thelma, Steve, and Pete. 
Among them, they hold eight different jobs. 
Each holds exactly two jobs. 
The jobs are chef, guard, nurse, clerk, police officer (gender not implied), teacher, actor, and boxer. 
The job of nurse is held by a male. 
The husband of the chef is the clerk. 
Roberta is not a boxer. 
Pete has no education past the ninth grade. 
Roberta, the chef, and the police officer went golfing together. 

Question: Who holds which jobs?

(from: http://www.mcs.anl.gov/~wos/mathproblems/jobs.html)
-}

data People = Roberta | Thelma | Steve | Pete

data Jobs = Chef | Guard | Nurse | Clerk
          | PoliceOfficer | Teacher | Actor | Boxer

main |
    all isPerson jobs             -- Each job belongs to a person
 && all (\p -> numJobs p jobs == 2) people -- Each holds exactly two jobs
 && actor == aMale                -- An actor is male
 && nurse == aMale                -- The job of nurse is held by a male. 
 && chef==aFemale && clerk==aMale -- The husband of the chef is the clerk. 
 && boxer /= Roberta              -- Roberta is not a boxer. 
 && teacher /= Pete               -- Pete has no education past the ninth grade.
 && nurse/=Pete && policeOfficer/=Pete -- (required for teacher,nurse,officer)
 && Roberta/=chef && Roberta/=policeOfficer && chef/=policeOfficer
    -- Roberta, the chef, and the police officer went golfing together. 
  = jobs
 where
   jobs@[chef,guard,nurse,clerk,policeOfficer,teacher,actor,boxer] = unknown
   people = [Roberta,Thelma,Steve,Pete]
--> [Thelma,Roberta,Steve,Pete,Steve,Roberta,Pete,Thelma]

isPerson p | p==aPerson = True
aPerson = Roberta ? Thelma ? Steve ? Pete
aMale   = Steve ? Pete
aFemale = Roberta ? Thelma

-- Number of jobs of a person:
numJobs p = length . filter (==p)
