module Encounter where
import Character

--helper function for turning list of characters into encounter info datatype
parseToEncounterInfo :: [(ClassSkills, Character)] -> NetworkPacket
parseToEncounterInfo fullInfo = LoadPacket classSkills turnOrd
    where
        classSkills = [fst curChar | curChar <- fullInfo]
        turnOrd = getTurnOrder [snd curChar | curChar <- fullInfo]

encounterTutorial :: NetworkPacket
encounterTutorial =  parseToEncounterInfo [getGod, getRat]


-- Warrior, Cleric
-- Haskeleton, Rat
encounter1 :: NetworkPacket
encounter1 = parseToEncounterInfo [getWarrior "Zev", getCleric "David", getHaskeleton, getRat]

--1x warrior, 1x mage, 1x cleric, 1x rogue
--2x rogue, 2x mage
encounter2 :: NetworkPacket
encounter2 = parseToEncounterInfo
    [getWarrior "Stark", getMage "Fern", getCleric "Frieren", getRogue "Sein",
    evilRogue "Wirbel", evilRogue "Ubel", evilMage "Aura", evilMage "Lugner"]

getEncounter :: String -> NetworkPacket
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

getGod :: (ClassSkills, Character)
getGod = (skills, char)
    where
        skills = ClassSkills "God" True True True True True True True True True True True True
        char = makeFriend "God" (ItemList 1 1 1 1 1 1) 100 100 100

getWarrior :: String -> (ClassSkills, Character)
getWarrior na = (skills, char)
    where
        skills = ClassSkills na True False False True False True False True False False False False
        char = makeFriend na (ItemList 2 1 0 0 1 1) 100 30 60

getCleric :: String -> (ClassSkills, Character)
getCleric na = (skills, char)
    where
        skills = ClassSkills na False True True False False False False False False True False True
        char = makeFriend na (ItemList 1 3 0 2 0 0) 60 100 40

getRogue :: String -> (ClassSkills, Character)
getRogue na = (skills, char)
    where
        skills = ClassSkills na True False False False True False True False False False False False
        char = makeFriend na (ItemList 2 1 2 0 1 0) 80 40 100

getMage :: String -> (ClassSkills, Character)
getMage na = (skills, char)
    where
        skills = ClassSkills na False True False False False False False False True False True False
        char = makeFriend na (ItemList 1 2 0 1 0 0) 50 100 30


---------------
--  Enemies  --
---------------

getRat :: (ClassSkills, Character)
getRat = (skills, char)
    where
        na = "Rat"
        skills = ClassSkills na True False False False False False False False False False False False
        char = makeEnemy na 10 10 10

getHaskeleton :: (ClassSkills, Character)
getHaskeleton = (skills, char)
    where
        na = "Haskeleton"
        skills = ClassSkills na True False False False False False True False False False False False
        char = makeEnemy na 50 20 50

evilRogue :: String -> (ClassSkills, Character)
evilRogue na = (skills, char)
    where
        skills = ClassSkills na True False False False True False True False False False False False
        char = makeEnemy na 80 40 100

evilMage :: String -> (ClassSkills, Character)
evilMage na = (skills, char)
    where
        skills = ClassSkills na False True False False False False False False True False True False
        char = makeEnemy na 50 100 30