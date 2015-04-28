{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}
import List (sum)

data Struct = Struct Int Example
  deriving (Eq,Show)

getEx :: Struct -> Example
getEx (Struct _ e) = e

data Example = One | Two | Three
  deriving (Eq,Show)

table1 n = zip (listN n) (repeat 0)

updateTable :: Struct -> [(Int,Int)] -> [(Int,Int)]
updateTable (Struct i v) ((j,w):ts)
  | i == j    = (i,w + exToInt v) : ts
  | otherwise = (j,w) : updateTable (Struct i v) ts
updateTable _ [] = []

exToInt :: Example -> Int
exToInt One = 1
exToInt Two = 2
exToInt Three = 3

listN :: Int -> [Int]
listN n = [1..n]

struct :: Int -> Struct
struct i = Struct i _

nOf_ :: Int -> [a] -> [a]
nOf_ 0 [] = []
nOf_ n (x:xs) | n == 0    = []
              | otherwise = x : nOf_ (n-1) xs ? nOf_ n xs

posNonDet :: [a] -> [a]
posNonDet (x:y:z:rs) = (x ? y ? z) : posNonDet rs
posNonDet [x,y]  = x : posNonDet [y]
posNonDet [y] = [y]

test1 n = (resList, newTable) |>  -- True
  not (null (thereExist 2 resList `suchThat` all (const True)))
 where
  resList :: [Struct]
  resList = map struct (listN n)
  newTable = foldr updateTable (table1 n) resList
  res = sum (map (exToInt . getEx) resList)

test2 n = (resList,res) |>
  not (null (thereExist 2 resList
              `suchThat` \pair -> res pair >= 4))
 where
  resList = map struct (listN n)
  res = sum . map (exToInt . getEx)

suchThat :: a -> (a -> Bool) -> a
suchThat x p | p x = x

(|>) :: a -> Bool -> a
x |> b | b = x

thereExist :: Int -> [a] -> [a]
thereExist = nOf_

test3 = (list, sum (map exToInt list))
      |> not (null (nOf_ 2 list `suchThat` const True))
 where
  list :: [Example]
  list = [_,_]