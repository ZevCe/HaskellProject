module Class where

import Character
import Item

data Class =
    Class {
        character :: Character,
        items     :: ItemList,
        name      :: String,
        team      :: String,
        stamAttack   :: Bool,
        kiAttack     :: Bool,
        heal         :: Bool,
        rally        :: Bool,
        invigorate   :: Bool,
        demoralize   :: Bool,
        intimidate   :: Bool,
        shield       :: Bool,
        amplify      :: Bool,
        dampen       :: Bool,
        curse        :: Bool,
        barrier      :: Bool}

instance Eq Class where
    c1 == c2 = name c1 == name c2

instance Ord Class where
    compare c1 c2 = compare (character c1) (character c2)

instance Show Class where
    show c = name c ++ "\n" ++ show (character c)

--functions for changing the character or items of a class
updateCharacter :: Class -> Character -> Class
updateCharacter char newChar =
     Class newChar (items char) (name char) (team char)
    (stamAttack char) (kiAttack char) (heal char) (rally char)
    (invigorate char) (demoralize char) (intimidate char) (shield char)
    (amplify char) (dampen char) (curse char) (barrier char)

updateItems :: Class -> ItemList -> Class
updateItems char newItems =
    Class (character char) newItems (name char) (team char)
    (stamAttack char) (kiAttack char) (heal char) (rally char)
    (invigorate char) (demoralize char) (intimidate char) (shield char)
    (amplify char) (dampen char) (curse char) (barrier char)