# Implementation of the [Jobs Puzzle] in Curry

by Sebastian Fischer, January 2015

## Verbal problem description

 > There are four people: Roberta, Thelma, Steve, and Pete.
 >
 > Among them, they hold eight different jobs.
 > 
 > Each holds exactly two jobs.
 > 
 > The jobs are chef, guard, nurse, clerk, police officer (gender not 
 > implied), teacher, actor, and boxer.
 >
 > The job of nurse is held by a male.
 >
 > The husband of the chef is the clerk.
 >
 > Roberta is not a boxer.
 > 
 > Pete has no education past the ninth grade.
 > 
 > Roberta, the chef, and the police officer went golfing together.
 > 
 > Question: Who holds which jobs?

## Solution

To find an answer, model people as data with certain properties, namely,
their jobs and gender.

> data Job          = chef | guard | nurse | clerk
>                   | officer | teacher | actor | boxer
>                                                   deriving (Eq, Ord, Show)
>
> Job_              = chef ? guard ? nurse ? clerk
>                   ? officer ? teacher ? actor ? boxer
>
>
> data Gender       = female | male                 deriving (Eq, Show)
> Gender_           = female ? male
>
>
> data Person       = Person    { jobs      :: [Job]
>                               , gender    :: Gender }
>                                                   deriving Show
>
> Person_           = Person    { jobs      = [Job_, Job_]
>                               , gender    = Gender_ }
>
>
> solution :: [Person]
> solution = people `accordingTo` description
>   where
>     people    = [roberta, thelma, steve, pete]
>     roberta   = Person_; thelma = Person_; steve = Person_; pete = Person_

### Formal problem description

In order to define `description`, model implicit and explicit knowledge.

#### Implicit Knowledge

Assume that the jobs of nurse and police officer require education past the
ninth grade.

>     pastNinthGradeJobs = [nurse, officer]

Further assume that people that go golfing together are different people. The
following predicate expresses that two jobs are held by different people or,
equivalently, no single person holds both jobs.

>     differentPeople job1 job2 =
>         forEvery (\person ->
>           not ((job1 `elem` jobs person) && (job2 `elem` jobs person)))

The following predicate on jobs is based on the outdated assumption that
married people are of different gender and husbands are male.

>     woman `hasHusband` man =
>         forEvery (\person -> 
>           ((woman `elem` jobs person) ==> (gender person == female)) &&
>           ((  man `elem` jobs person) ==> (gender person ==   male)))
>         

The following (similarly outdated) assumptions are also implicit parts of
`description`.

>     description =
>      -- A person's gender can be inferred from their name.
>         gender roberta    == female   &&
>         gender thelma     == female   &&
>         gender steve      ==   male   &&
>         gender pete       ==   male   &&
>
>      -- Certain job names, like actor and actress, imply gender.
>         forEvery (\person ->
>           (actor `elem` jobs person) ==> (gender person == male)) &&

#### Explicit knowledge

Now formalize the verbal description.

>      -- Among them they hold eight different jobs.
>         allDifferent (concatMap jobs people) &&
>
>      -- Each holds exactly two jobs.
>         forEvery (\person -> (jobs person!!0) < (jobs person!!1)) &&
>
>      -- The job of nurse is held by a male.
>         forEvery (\person -> 
>           (nurse `elem` jobs person) ==> (gender person == male)) &&
>
>      -- The husband of the chef is the clerk.
>         (chef `hasHusband` clerk) &&
>
>      -- Roberta is not a boxer.
>         not (boxer `elem` jobs roberta) &&
>
>      -- Pete has no education past the ninth grade.
>         allDifferent (jobs pete ++ pastNinthGradeJobs) &&
>
>      -- Roberta, the chef, and the police officer went golfing together.
>         allDifferent (jobs roberta ++ [chef, officer]) &&
>         differentPeople chef officer

### Helper functions

>     forEvery p = all p people
>
>
> accordingTo :: a -> Bool -> a
> a `accordingTo` b | b = a
>
> (==>) :: Bool -> Bool -> Bool
> a ==> b = not a || b
>
> allDifferent :: Eq a => [a] -> Bool
> allDifferent []       = True
> allDifferent (x:xs)   = all (/=x) xs && allDifferent xs

## Printing the unique solution

> main = putStr (unlines
>    [ "Roberta" ++ showJobs roberta
>    , "Thelma"  ++ showJobs thelma
>    , "Steve"   ++ showJobs steve
>    , "Pete"    ++ showJobs pete
>    ])
>   where
>     [roberta, thelma, steve, pete]:_ = findall (\s -> s =:= solution)
>
>     showJobs p =
>       " is " ++ show (jobs p!!0) ++ " and " ++ show (jobs p!!1) ++ "."

Execute this program using the experimental type-class branch of the [Münster
Curry Compiler]. It prints the following after a few seconds.

~~~
Roberta is guard and teacher.
Thelma is chef and boxer.
Steve is nurse and officer.
Pete is clerk and actor.
~~~

[Jobs Puzzle]: http://www.mcs.anl.gov/~wos/mathproblems/jobs.html
[mail@sebfisch.de]: mailto:mail@sebfisch.de
[Münster Curry Compiler]: http://danae.uni-muenster.de/~lux/curry/
