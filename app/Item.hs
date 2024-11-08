module Item
( useHealthPotion ) where

import Character
import ModifyStats

useHealthPotion :: Character -> Character
useHealthPotion c =
    if stamina c + 20 > maxStamina c 
        then modifyStamina c (maxStamina c - stamina c)
    else modifyStamina c 20