module Item
( useHealthPotion ) where

import Character

--at some point rework functions in this file to take in and return list of items
data ItemList =
    ItemList { numHealthPotions  :: Int,
               numManaPotions    :: Int,
               numThrowingKnives :: Int,
               numMagicalSeals   :: Int,
               numWebTraps       :: Int,
               numHastePotions   :: Int}



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

useManaPotion user target items = 
    if numManaPotions items /= 0
        then (modifyKi target restoreAmt, usedItems)
    else (target, items)
    where
        usedItems =
            ItemList (numHealthPotions items) (numManaPotions items - 1)
            (numThrowingKnives items) (numMagicalSeals items)
            (numWebTraps items) (numHastePotions items)
        
        restoreAmt = min 20 $ maxKi target - ki target

useThrowingKnives target items = 
    if numThrowingKnives items /= 0
        then (modifyStamina target (-20), usedItems)
    else (target, items)
    where
        usedItems =
            ItemList (numHealthPotions items) (numManaPotions items)
            (numThrowingKnives items - 1) (numMagicalSeals items)
            (numWebTraps items) (numHastePotions items)

useMagicalSeal target items = 
    if numThrowingKnives items /= 0
        then (modifyKi target (-20), usedItems)
    else (target, items)
    where
        usedItems =
            ItemList (numHealthPotions items) (numManaPotions items)
            (numThrowingKnives items) (numMagicalSeals items - 1)
            (numWebTraps items) (numHastePotions items)

useWebTraps target items = 
    if "webTrap" notElem (statuses target)
        then (newTarget, usedItems)
    else (target, items) 
    where
        usedItems =
            ItemList (numHealthPotions items) (numManaPotions items)
            (numThrowingKnives items) (numMagicalSeals items)
            (numWebTraps items - 1) (numHastePotions items)

        slowedTarget = modifySpeed target -(speed target / 2)
        newTarget = modifyStatuses target ("webTrap" : statuses target)

useHastePotion target items =
    if "hastePotion" notElem (statuses target)
        then (newTarget, usedItems)
    else (target, items) 
    where
        usedItems =
            ItemList (numHealthPotions items) (numManaPotions items)
            (numThrowingKnives items) (numMagicalSeals items)
            (numWebTraps items) (numHastePotions items - 1)

        hastedTarget = modifySpeed target (speed target)
        newTarget = modifyStatuses target ("hastePotion" : statuses target)