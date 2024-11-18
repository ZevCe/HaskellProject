module Item where

import Character

--at some point rework functions in this file to take in and return list of items
data ItemList =
    ItemList { numHealthPotions  :: Int,
               numManaPotions    :: Int,
               numThrowingKnives :: Int,
               numMagicalSeals   :: Int,
               numWebTraps       :: Int,
               numHastePotions   :: Int}

instance Show ItemList where
    show i = "Health Potions: " ++ show (numHealthPotions i) ++ 
        "\nMana Potions: " ++ show (numManaPotions i) ++ 
        "\nThrowing Knives" ++ show (numThrowingKnives i) ++
        "\nMagical Seals" ++ show (numMagicalSeals i) ++
        "\nWeb Traps" ++ show (numWebTraps i) ++ 
        "\nHaste Potions" ++ show (numHastePotions i) 

useHealthPotion :: Character -> ItemList -> (Character, ItemList)
useHealthPotion target items =
    if numHealthPotions items /= 0
        then (modifyStamina target restoreAmt, usedItems)
    else (target, items)
    where
        usedItems =
            ItemList (numHealthPotions items - 1) (numManaPotions items)
            (numThrowingKnives items) (numMagicalSeals items)
            (numWebTraps items) (numHastePotions items)

        restoreAmt = min 20 $ maxStamina target - stamina target

useManaPotion :: Character -> ItemList -> (Character, ItemList)
useManaPotion target items =
    if numManaPotions items /= 0
        then (modifyKi target restoreAmt, usedItems)
    else (target, items)
    where
        usedItems =
            ItemList (numHealthPotions items) (numManaPotions items - 1)
            (numThrowingKnives items) (numMagicalSeals items)
            (numWebTraps items) (numHastePotions items)

        restoreAmt = min 20 $ maxKi target - ki target

useThrowingKnives :: Character -> ItemList -> (Character, ItemList)
useThrowingKnives target items =
    if numThrowingKnives items /= 0
        then (modifyStamina target (-20), usedItems)
    else (target, items)
    where
        usedItems =
            ItemList (numHealthPotions items) (numManaPotions items)
            (numThrowingKnives items - 1) (numMagicalSeals items)
            (numWebTraps items) (numHastePotions items)

useMagicalSeal :: Character -> ItemList -> (Character, ItemList)
useMagicalSeal target items =
    if numThrowingKnives items /= 0
        then (modifyKi target (-20), usedItems)
    else (target, items)
    where
        usedItems =
            ItemList (numHealthPotions items) (numManaPotions items)
            (numThrowingKnives items) (numMagicalSeals items - 1)
            (numWebTraps items) (numHastePotions items)

useWebTraps :: Character -> ItemList -> (Character, ItemList)
useWebTraps target items =
    if "webTrap" `notElem` statuses target
        then (newTarget, usedItems)
    else (target, items)
    where
        usedItems =
            ItemList (numHealthPotions items) (numManaPotions items)
            (numThrowingKnives items) (numMagicalSeals items)
            (numWebTraps items - 1) (numHastePotions items)

        slowedTarget = modifySpeed target (- (speed target `div` 2))
        newTarget = modifyStatuses slowedTarget ("webTrap" : statuses target)

useHastePotion :: Character -> ItemList -> (Character, ItemList)
useHastePotion target items =
    if "hastePotion" `notElem` statuses target
        then (newTarget, usedItems)
    else (target, items)
    where
        usedItems =
            ItemList (numHealthPotions items) (numManaPotions items)
            (numThrowingKnives items) (numMagicalSeals items)
            (numWebTraps items) (numHastePotions items - 1)

        hastedTarget = modifySpeed target (speed target)
        newTarget = modifyStatuses hastedTarget ("hastePotion" : statuses target)