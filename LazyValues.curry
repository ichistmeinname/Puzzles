import SetFunctions (foldValues,mapValues,set2, isEmpty)

andL,andV :: Bool
andL = foldr (&&) True (map not (replicate 1000000 False))

andV = -- foldValues (&&) True 
  isEmpty (set2 replicateND 1000000 True)

replicateND n v =
  if n==0 then failed
          else v ? replicateND (n-1) v