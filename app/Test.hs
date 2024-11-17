module Test where

import Character
import Actions
import Item
import Data.List (sortBy)
import Data.Ord (comparing)
import Test.HUnit

main = let 
        p1 = makeCharacter 100 50 20 "Enemy"
        p2 = makeCharacter 80 50 40 "Friend"
        p3 = makeCharacter 90 70 20 "Friend"
        p4 = makeCharacter 85 65 30 "Friend"
        p5 = makeCharacter 50 95 70 "Friend"
            in
                do
                    print p1
                    print p2
                    print "p2 using health potion"
                    print $ useHealthPotion p2
                    print "p1 stamina attacking p2 at level 0"
                    print $ staminaSingleAttack p1 p2 0
                    print "p1 stamina attacking p2 at level 1"
                    print $ staminaSingleAttack p1 p2 1
                    print "p1 stamina attacking p2 at level 2"
                    print $ staminaSingleAttack p1 p2 2
                    print "p1 stamina attacking p2 at level 3"
                    print $ staminaSingleAttack p1 p2 3
                    print "p2 ki attacking p1 at level 0"
                    print $ kiSingleAttack p2 p1 0
                    print "p2 ki attacking p1 at level 1"
                    print $ kiSingleAttack p2 p1 1
                    print "p2 ki attacking p1 at level 2"
                    print $ kiSingleAttack p2 p1 2
                    print "p2 ki attacking p1 at level 3"
                    print $ kiSingleAttack p2 p1 3
                    print "p1 stamina attacking [p2, p3, p4, p5] at level 1"
                    print $ staminaGroupAttack p1 [p2, p3, p4, p5] 1

testItems :: Test
testItems = TestCase $ do
    let p1 = makeCharacter 100 50 20 "Friend"
        p2 = makeCharacter 100 50 20 "Friend"
        p3 = makeCharacter 100 50 20 "Enemy"
    print $ staminaSingleAttack p3 p1 3
    print $ useHealthPotion p1
    assertEqual "Health Potion Test" p1 p2

-- Param: characters - List of all characters in a battle
-- Returns: The list of characters sorted by speed (descending)
-- Bugs: Doesn't take into consideration team, but if we pass in enemy before
--  friendly, we shouldn't need to do that (more comprehensive testing) needed.
getTurnOrder :: [Character] -> [Character]
getTurnOrder characters = reverse $ sortBy (comparing speed) characters