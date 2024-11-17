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
    if not (numHealthPotions items == 0)
        then if stamina target + 20 > maxStamina target
                then (modifyStamina target (maxStamina target - stamina target), usedItems)
             else (modifyStamina target 20, usedItems)
    else (target, items)
    where
        usedItems =
            ItemList (numHealthPotions items - 1) (numManaPotions items)
            (numThrowingKnives items) (numMagicalSeals items)
            (numWebTraps items) (numHastePotions items)



useManaPotion user target items = undefined

useThrowingKnives user target items = undefined

useMagicalSeal user target items = undefined

useWebTraps user target items = undefined

useHastePotion user target items = undefined