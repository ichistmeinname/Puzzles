module Cuzzle (jobsPuzzle) where

data Job = Actor
         | Boxer
         | Chef
         | Clerk
         | Nurse
         | Guard
         | PoliceOfficer
         | Teacher

perm :: [a] -> [a]
perm []     = []
perm (x:xs) = insert x (perm xs)

insert :: a -> [a] -> [a]
insert x xs = x:xs
insert x (y:ys) = y : insert x ys

isSorted :: [a] -> Bool
isSorted []       = True
isSorted [_]      = True
isSorted (x:y:ys) = x < y && isSorted (y:ys)

jobsPuzzle :: [[Job]]
jobsPuzzle =
  -- Erzeugung aller Möglichkeiten
  let jobs             = perm [Actor,Boxer,Chef,Clerk,Nurse,Guard,PoliceOfficer,Teacher]
      (male,female)    = splitAt 4 jobs
      (steve,pete)     = splitAt 2 male
      (roberta,thelma) = splitAt 2 female in

  -- Krankenpfleger und Schauspieler sind Männer
  if all (`elem` male) [Nurse,Actor] &&

  -- Der Ehemann der Köchin ist Büroangestellter
     Chef `elem` female && Clerk `elem` male &&

  -- Roberta ist nicht Boxer von Beruf und auch nicht Köchin oder Polizistin,
  -- da sie mit diesen zum Golf spielen war
     all (`notElem` roberta) [Boxer,Chef,PoliceOfficer] &&

  -- Pete hat nur einen Hauptschulabschluss und ist somit für die Berufe
  -- des Krankenpfleger des Polizisten sowie des Lehrers nicht qualifiziert.
     all (`notElem` pete) [Nurse,PoliceOfficer,Teacher] &&

  -- Roberta, die Köchin und der/die Polizist/in waren zusammen Golf spielen.
  -- Das bedeutet der Beruf des Koches und der des Polizisten kann nicht von
  -- der gleichen Person ausgeübt werden und diese Person ist nicht Roberta.
     any (`notElem` pete) [PoliceOfficer,Chef] &&
     any (`notElem` steve) [PoliceOfficer,Chef] &&
     any (`notElem` thelma) [PoliceOfficer,Chef] &&

  -- sortieren der Teillisten
     isSorted roberta && isSorted thelma && isSorted steve && isSorted pete

  -- Lösung: [[Robertas Jobs],[Thelmas Jobs],[Steves Jobs],[Petes Jobs]]
  then [roberta,thelma,steve,pete]
  else failed

-- JobsPuzzle> jobsPuzzle
-- [[Guard,Teacher],[Boxer,Chef],[Nurse,PoliceOfficer],[Actor,Clerk]]
-- More values? [Y(es)/n(o)/a(ll)] a
-- No more solutions.

