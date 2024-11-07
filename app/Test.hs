import Character
import Item
import Data.List (sortBy)
import Data.Ord (comparing)

main = let 
        p1 = Character 100 50 20 "Enemy"
        p2 = Character 30 50 40 "Friend"
            in
                do
                    print p1
                    print p2
                    print "Using health potion"
                    print $ useHealthPotion p2

-- Param: characters - List of all characters in a battle
-- Returns: The list of characters sorted by speed (descending)
-- Bugs: Doesn't take into consideration team, but if we pass in enemy before
--  friendly, we shouldn't need to do that (more comprehensive testing) needed.
getTurnOrder :: [Character] -> [Character]
getTurnOrder characters = reverse $ sortBy (comparing speed) characters