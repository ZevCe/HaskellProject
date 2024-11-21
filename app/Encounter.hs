module Encounter where

import Character
import Class
import Item

getEncounterTutorial :: [Class]
getEncounterTutorial = [getGod, getRat]

-- Warrior, Cleric
-- Haskeleton, Rat
getBasicEncounter1 :: [Class]
getBasicEncounter1 = [getWarrior "Zev", getCleric "David", getHaskeleton, getRat]

--1x warrior, 1x mage, 1x cleric, 1x rogue
--2x rogue, 2x mage
getBasicEncounter2 :: [Class]
getBasicEncounter2 = [getWarrior "Stark", getMage "Fern", getCleric "Frieren", getRogue "Sein", evilRogue "Wirbel", evilRogue "Ubel", evilMage "Aura", evilMage "Lugner"]

-- Doesn't take into account invalid names, maybe a ST fix later
-- Class
-- makeCharacter Stamina Ki Speed
--  ItemList Health, Mana, Knives, Seals, Web, Haste
--  "Name" "Faction"
--  stam ki heal rally invigorate demoralize
--  Intimidate shield amplify dampen curse barrier


--------------
--  Allies  --
--------------

getGod :: Class
getGod = Class
            (makeCharacter 100 100 100)
            (ItemList 1 1 1 1 1 1)
            "God" "Friend"
            True True True True True True
            True True True True True True

getWarrior :: String -> Class
getWarrior tname = Class
                    (makeCharacter 100 30 60)
                    (ItemList 2 1 0 0 1 1)
                    tname "Friend"
                    True False False True False True
                    False True False False False False

getCleric :: String -> Class
getCleric tname = Class
                    (makeCharacter 60 100 40)
                    (ItemList 1 3 0 2 0 0)
                    tname "Friend"
                    False True True False False False
                    False False False True False True

getRogue :: String -> Class
getRogue tname = Class
                    (makeCharacter 80 40 100)
                    (ItemList 2 1 2 0 1 0)
                    tname "Friend"
                    True False False False True False
                    True False False False False False

getMage :: String -> Class
getMage tname = Class
                    (makeCharacter 50 100 30)
                    (ItemList 1 2 0 1 0 0)
                    tname "Friend"
                    False True False False False False
                    False False True False True False

---------------
--  Enemies  --
---------------

getRat :: Class
getRat = Class
            (makeCharacter 10 10 10)
            (ItemList 0 0 0 0 0 0)
            "Rat" "Enemy"
            True False False False False False
            False False False False False False

getHaskeleton :: Class
getHaskeleton = Class
                (makeCharacter 50 20 50)
                (ItemList 0 0 1 0 0 0)
                "Haskeleton" "Enemy"
                True False False False False False                
                True False False False False False


evilRogue :: String -> Class
evilRogue tname = Class
                    (makeCharacter 80 40 100)
                    (ItemList 2 1 2 0 1 0)
                    tname "Enemy"
                    True False False False True False
                    True False False False False False

evilMage :: String -> Class
evilMage tname = Class
                    (makeCharacter 50 100 30)
                    (ItemList 1 2 0 1 0 0)
                    tname "Enemy"
                    False True False False False False
                    False False True False True False

