module Encounter where

import Character

encounterTutorial :: [Character]
encounterTutorial = getTurnOrder [getGod, getRat]

-- Warrior, Cleric
-- Haskeleton, Rat
encounter1 :: [Character]
encounter1 = getTurnOrder [getWarrior "Zev", getCleric "David", getHaskeleton, getRat]

--1x warrior, 1x mage, 1x cleric, 1x rogue
--2x rogue, 2x mage
encounter2 :: [Character]
encounter2 = getTurnOrder
    [getWarrior "Stark", getMage "Fern", getCleric "Frieren", getRogue "Sein", 
    evilRogue "Wirbel", evilRogue "Ubel", evilMage "Aura", evilMage "Lugner"]

getEncounter :: String -> [Character]
getEncounter input
     | input == "1" = encounter1
     | input == "2" = encounter2
     | otherwise = encounterTutorial

-- Doesn't take into account invalid names, maybe a ST fix later
-- leaving this block below here for reference in case james forgot which chars had which abilities
--  stam ki heal rally invigorate demoralize
--  Intimidate shield amplify dampen curse barrier

{- new and easier way to make chars, found over in Character.hs 
makeFriend :: String -> ItemList -> Int -> Int -> Int -> Character
makeFriend na ite sta k spd =
    Character na "Friend" ite sta sta k k spd []

makeEnemy :: String -> Int -> Int -> Int -> Character
makeEnemy na sta k spd =
    Character na "Enemy" (ItemList 0 0 0 0 0 0) sta sta k k spd []
-}

--------------
--  Allies  --
--------------

getGod :: Character
getGod = makeFriend "God" (ItemList 1 1 1 1 1 1) 100 100 100
                        
getWarrior :: String -> Character
getWarrior na = makeFriend na (ItemList 2 1 0 0 1 1) 100 30 60
--True False False True False True
--False True False False False False

                    
getCleric :: String -> Character
getCleric na = makeFriend na (ItemList 1 3 0 2 0 0) 60 100 40    
--False True True False False False             
--False False False True False True

getRogue :: String -> Character
getRogue na = makeFriend na (ItemList 2 1 2 0 1 0) 80 40 100
--True False False False True False
--True False False False False False

getMage :: String -> Character
getMage na = makeFriend na (ItemList 1 2 0 1 0 0) 50 100 30
--False True False False False False
--False False True False True False

---------------
--  Enemies  --
---------------

getRat :: Character
getRat = makeEnemy "Rat" 10 10 10
--True False False False False False
--False False False False False False

getHaskeleton :: Character
getHaskeleton = makeEnemy "Haskeleton" 50 20 50
--True False False False False False                
--True False False False False False


evilRogue :: String -> Character
evilRogue na = makeEnemy na 80 40 100
--True False False False True False
--True False False False False False

evilMage :: String -> Character
evilMage na = makeEnemy na 50 100 30
--False True False False False False
--False False True False True False