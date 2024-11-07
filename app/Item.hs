module Item 
( useHealthPotion ) where

import Character

useHealthPotion (Character (Stamina sta) ki speed team) =
    Character (Stamina (sta + 20)) ki speed team