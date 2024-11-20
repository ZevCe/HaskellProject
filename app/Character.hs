module Character where

import Data.List (sortBy)
import Data.Ord (comparing)

data Character  =
    Character { stamina    :: Int,
                maxStamina :: Int,
                ki         :: Int,
                maxKi      :: Int,
                speed      :: Int,
                statuses   :: [String]}


instance Eq Character where
    c1 == c2 = (speed c1) == (speed c2)

instance Ord Character where
    compare c1 c2 = compare (speed c1) (speed c2)

instance Show Character where
    show c =
        "Stamina: " ++ show (stamina c) ++ "/" ++ show (maxStamina c) ++
        "\nKi: " ++ show (ki c) ++ "/" ++ show (maxKi c) ++
        "\nSpeed: " ++ show (speed c) ++
        "\nStatuses: " ++ show (statuses c)

-- Param: characters - List of all characters in a battle
-- Returns: The list of characters sorted by speed (descending)
getTurnOrder :: [Character] -> [Character]
getTurnOrder = sortBy (flip (comparing speed))

makeCharacter :: Int -> Int -> Int -> Character
makeCharacter sta k spd =
    Character sta sta k k spd []

modifyStamina :: Character -> Int -> Character
modifyStamina char amt =
    Character (stamina char + amt) (maxStamina char) (ki char) (maxKi char) (speed char) (statuses char)

modifyKi :: Character -> Int -> Character
modifyKi char amt =
    Character (stamina char) (maxStamina char) (ki char + amt) (maxKi char) (speed char) (statuses char)

modifySpeed :: Character -> Int -> Character
modifySpeed char amt =
    Character (stamina char) (maxStamina char) (ki char) (maxKi char) (speed char + amt) (statuses char)

modifyStatuses :: Character -> [String] -> Character
modifyStatuses char newStatuses =
    Character (stamina char) (maxStamina char) (ki char) (maxKi char) (speed char) newStatuses