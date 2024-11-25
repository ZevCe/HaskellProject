{-# LANGUAGE DeriveGeneric #-}
module Character where

import Data.List
import Data.Ord
import GHC.Generics
import Data.Aeson


data Character  =
    Character { name       :: String,
                team       :: String,
                items      :: ItemList,
                stamina    :: Int,
                maxStamina :: Int,
                ki         :: Int,
                maxKi      :: Int,
                speed      :: Int,
                statuses   :: [String]}
                deriving(Generic)


instance Eq Character where
    c1 == c2 = (name c1) == (name c2)

instance Ord Character where
    compare c1 c2 = compare (speed c1) (speed c2)

instance Show Character where
    show c =
        "Stamina: " ++ show (stamina c) ++ "/" ++ show (maxStamina c) ++
        "\tKi: " ++ show (ki c) ++ "/" ++ show (maxKi c) ++
        "\tSpeed: " ++ show (speed c) ++
        "\tStatuses: " ++ show (statuses c) ++
        "\tHealth Potions: " ++ show (numHealthPotions $ items c) ++
        "\tMana Potions: " ++ show (numManaPotions $ items c) ++
        "\tThrowing Knives: " ++ show (numThrowingKnives $ items c) ++
        "\tMagical Seals: " ++ show (numMagicalSeals $ items c) ++
        "\tWeb Traps: " ++ show (numWebTraps $ items c) ++
        "\tHaste Potions: " ++ show (numHastePotions $ items c)

instance FromJSON Character
instance ToJSON Character

data ItemList =
    ItemList { numHealthPotions  :: Int,
               numManaPotions    :: Int,
               numThrowingKnives :: Int,
               numMagicalSeals   :: Int,
               numWebTraps       :: Int,
               numHastePotions   :: Int}
               deriving(Generic)

instance FromJSON ItemList
instance ToJSON ItemList

--action can either be the action for the back end to perform
--or the log of updates to display to the player for the front end
--incoming and outgoing packets may eventually be different, just for initial testing they are the same
data NetworkPacket = 
    NetworkPacket {
        action :: [String],
        turnOrder  :: [Character]
    }
    deriving(Generic, Show)
instance FromJSON NetworkPacket
instance ToJSON NetworkPacket

--returns the list of characters sorted by speed (descending)
getTurnOrder :: [Character] -> [Character]
getTurnOrder = sortBy (comparing Down)

--replace characters from the old list with characters from the new list
--filtering out all the dead characters then re-organizing them into turn order
updateCharList :: [Character] -> [Character] -> [Character]
updateCharList oldList updatedChars = getTurnOrder aliveChars
    where
        unaffectedChars = oldList \\ updatedChars
        aliveChars = filter (\c -> ki  c > 0 && stamina c > 0) (updatedChars ++ unaffectedChars)

--finding a target amongst a list of Characters
getTarget :: [Character] -> String -> Character
getTarget chars targetName = head $ filter (\c -> name c == targetName) chars

--getting characters on a specific team
enemyTeam :: [Character] -> [Character]
enemyTeam = filter (\c -> team c == "Enemy")

friendTeam :: [Character] -> [Character]
friendTeam = filter (\c -> team c == "Friend")

-- --after we re-organize everyone by speed we need to re-iterate back through
-- --whose turn it currently is
-- fixTurnOrder :: Class -> [Class] -> [Class]
-- fixTurnOrder turnChar (char:chars) =
--     if turnChar == char then chars ++ [char]
--     else fixTurnOrder turnChar (chars ++ [char])
-- fixTurnOrder _ _ = undefined

--quicker way of making a new character
makeFriend :: String -> ItemList -> Int -> Int -> Int -> Character
makeFriend na ite sta k spd =
    Character na "Friend" ite sta sta k k spd []

makeEnemy :: String -> Int -> Int -> Int -> Character
makeEnemy na sta k spd =
    Character na "Enemy" (ItemList 0 0 0 0 0 0) sta sta k k spd []

--shorthand ways to modify just one stat of a character
modifyItems :: Character -> ItemList -> Character
modifyItems char newItems =
    Character (name char) (team char) newItems (stamina char) (maxStamina char) (ki char) (maxKi char) (speed char) (statuses char)

modifyStamina :: Character -> Int -> Character
modifyStamina char amt =
    Character (name char) (team char) (items char) (stamina char + amt) (maxStamina char) (ki char) (maxKi char) (speed char) (statuses char)

modifyKi :: Character -> Int -> Character
modifyKi char amt =
    Character (name char) (team char) (items char) (stamina char) (maxStamina char) (ki char + amt) (maxKi char) (speed char) (statuses char)

modifySpeed :: Character -> Int -> Character
modifySpeed char amt =
    Character (name char) (team char) (items char) (stamina char) (maxStamina char) (ki char) (maxKi char) (speed char + amt) (statuses char)

modifyStatuses :: Character -> [String] -> Character
modifyStatuses char newStatuses = Character (name char) (team char) (items char) (stamina char) (maxStamina char) (ki char) (maxKi char) (speed char) newStatuses