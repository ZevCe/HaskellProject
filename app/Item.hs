module Item 
( useHealthPotion ) where

import Character

useHealthPotion (Character sta ki speed team) =
    Character (sta + 20) ki speed team