module Test where

import Character
import Actions
import Item
import Test.HUnit


testActions :: Test
testActions = TestCase $ do
        let p1 = makeCharacter 100 50 20
            p2 = makeCharacter 80 50 40 
            p3 = makeCharacter 90 70 20 
            p4 = makeCharacter 85 65 30 
            p5 = makeCharacter 50 95 70 

        print p1
        print p2
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

testHealthPotion :: Test
testHealthPotion = TestCase $ do
    let p1 = Character 80 100 50 50 20 []
        p2 = makeCharacter 100 50 20
        p3 = Character 90 100 50 50 20 []
        p4 = makeCharacter 100 50 20
        items = ItemList 2 2 2 2 2 2
    print $ useHealthPotion p1 items
    print $ useHealthPotion p3 items
    assertEqual "Full Health Potion Test" p1 p2
    assertEqual "Overflow Health Potion Test" p3 p4

testManaPotion :: Test
testManaPotion = TestCase $ do
    let p1 = Character 100 100 30 50 20 []
        p2 = makeCharacter 100 50 20
        p3 = Character 100 100 40 50 20 []
        p4 = makeCharacter 100 50 20
        items = ItemList 2 2 2 2 2 2
    print $ useManaPotion p1 items
    print $ useManaPotion p3 items

    assertEqual "Full Mana Potion Test" p1 p2
    assertEqual "Overflow Mana Potion Test" p3 p4

testThrowingKnives :: Test
testThrowingKnives = TestCase $ do
    let p1 = Character 80 100 50 50 20 []
        p2 = makeCharacter 100 50 20
        items = ItemList 2 2 2 2 2 2
    print $ useThrowingKnives p2 items

    assertEqual "Throwing Knives Test" p1 
    

