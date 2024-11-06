module Item 
( useHealthPotion ) where

import Character

useHealthPotion (Character (Stamina sta) k sp) =
    Character (Stamina (sta + 20)) k sp