module Test where
import Character
import Actions
import Encounter
import Test.HUnit
{-



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
-}

testHealthPotion :: Test
testHealthPotion = TestCase $ do
    let (_, full) = getWarrior "Target"
        half = Character "User" "Friend" (ItemList 2 1 0 0 1 1) 50 100 30 30 60 []
        first = performAction ["HeP",(name half)] [half, full]
        second = performAction ["HeP",(name full)] [full, half]
    assertEqual "Post Health Pot" (action first) [" health potion ","User stamina +20"]
    assertEqual "Post Health Pot Overflow" (action second) [" health potion ","Target stamina +0"]

testManaPotion :: Test
testManaPotion = TestCase $ do
    let (_, full) = getWarrior "Target"
        half = Character "User" "Friend" (ItemList 2 1 0 0 1 1) 100 100 10 30 60 []
        first = performAction ["MP",(name half)] [half, full]
        second = performAction ["MP",(name full)] [full, half]
    assertEqual "Post Mana Pot" (action first) [" mana potion ","User ki +20"]
    assertEqual "Post Mana Pot Overflow" (action second) [" mana potion ","Target ki +0"]

testWebTrap :: Test
testWebTrap = TestCase $ do
    let (userSkills, user) = getCleric "User"
        (_, target) = getRat
        first = performAction ["WT","Rat"] [user, target]
    assertEqual "Web Trap Test" (action first) [" web trap ","Rat web trapped (speed halved)","Rat speed --5"]

testThrowingKnives :: Test
testThrowingKnives = TestCase $ do
    let (userSkills, user) = getWarrior "User"
        (_, target) = getHaskeleton
        first = performAction ["TK","Haskeleton"] [user, target]
    assertEqual "Throwing Knives Test" (action first) [" throwing knives ","Haskeleton stamina -20"]

testMagicalSeal :: Test
testMagicalSeal = TestCase $ do
    let (userSkills, user) = getWarrior "User"
        (_, target) = getHaskeleton
        first = performAction ["MS","Haskeleton"] [user, target]
    assertEqual "Magical Seal Test" (action first) [" magical seals ","Haskeleton ki -20","Haskeleton perished"]

testStaminaAttack :: Test
testStaminaAttack = TestCase $ do
    let (userSkills, user) = getWarrior "User"
        (_, enemy1) = getRat
        (_, enemy2) = getHaskeleton
        (_, target) = getWarrior "Target"
        weak = Character "Weak" "Friend" (ItemList 2 1 0 0 1 1) 10 100 10 30 60 []
        first = performAction ["SA","1",(name target)] [user, target, weak]
        second = performAction ["SA","2",(name target)] [user, target, weak]
        third = performAction ["SA","3",(name target)] [user, target, weak]
        fourth = performAction ["SA","1", "A"] [user, target, weak, enemy1, enemy2]
        fifth = performAction ["SA","2", "A"] [user, target, weak, enemy1, enemy2]
    assertEqual "User Post SA Lv 1" ((action first) !! 0) "User stamina -10"
    assertEqual "Target Post SA Lv 1" ((action first) !! 1) "Target stamina -20"
    assertEqual "User Post SA Lv 2" ((action second) !! 0) "User stamina -25"
    assertEqual "Target Post SA Lv 2" ((action second) !! 1) "Target stamina -35"
    assertEqual "User Post SA Lv 3" ((action third) !! 0) "User stamina -40"
    assertEqual "Target Post SA Lv 3" ((action third) !! 1) "Target stamina -50"
    assertEqual "Group Post SA Lv 1" (action fourth) ["User stamina -30","Rat stamina -150","Rat has perished","Haskeleton stamina -30"]
    assertEqual "Group Post SA Lv 2" (action fifth) ["User stamina -60","Rat stamina -300","Rat has perished","Haskeleton stamina -60","Haskeleton has perished"]

testKiAttack :: Test
testKiAttack = TestCase $ do
    let (userSkills, user) = getWarrior "User"
        (_, target) = getWarrior "Target"
        (_, enemy1) = getRat
        (_, enemy2) = getHaskeleton
        first = performAction ["KA","1",(name target)] [user, target]
        second = performAction ["KA","2",(name target)] [user, target]
        third = performAction ["KA","3",(name target)] [user, target]
        fourth = performAction ["KA","1", "A"] [user, target, enemy1, enemy2]
        fifth = performAction ["KA","2", "A"] [user, target, enemy1, enemy2]
    assertEqual "User Post KA Lv 1" ((action first) !! 0) "User ki -10"
    assertEqual "Target Post KA Lv 1" ((action first) !! 1) "Target ki -20"
    assertEqual "User Post KA Lv 2" ((action second) !! 0) "User ki -25"
    assertEqual "Target Post KA Lv 2" ((action second) !! 1) "Target ki -35" 
    assertEqual "User Post KA Lv 3" ((action third) !! 0) "User ki -40"
    assertEqual "Target Post KA Lv 3" ((action third) !! 1) "Target ki -50"
    assertEqual "Group Post SA Lv 1" (action fourth) ["User ki -30","Rat ki -45","Rat has perished","Haskeleton ki -15"]
    assertEqual "Group Post SA Lv 2" (action fifth) ["User ki -60","Rat ki -90","Rat has perished","Haskeleton ki -30","Haskeleton has perished"]

testHeal :: Test
testHeal = TestCase $ do
    let weak = Character "Weak" "Friend" (ItemList 2 1 0 0 1 1) 10 100 10 30 60 []
        half = Character "Half" "Friend" (ItemList 2 1 0 0 1 1) 50 100 10 30 60 []
        (_, full) = getWarrior "Strong"
        first = performAction ["Hl","1", "Weak"] [full, weak]
        second = performAction ["Hl","2", "Weak"] [full, weak]
        third = performAction ["Hl","1", "Strong"] [weak, full]
        fourth = performAction ["Hl","3", "Weak"] [full, weak]
        fifth = performAction ["Hl","1", "A"] [full, weak, half]
        sixth = performAction ["Hl","2", "A"] [full, weak, half]
    assertEqual "Target Post Hl Lv 1" (action first) ["Strong ki -10","Weak ki 5"]
    assertEqual "Target Post Hl Lv 2" (action second) ["Strong ki -25","Weak ki 13"]
    assertEqual "Target Post Hl Lv 3" (action fourth) ["Strong ki -40","Weak ki 20"]
    assertEqual "Target Post Hl Lv 1 Overflow" (action third) ["Weak ki -10","Strong ki 0"]
    assertEqual "Group Post Hl Lv1 " (action fifth) ["Strong ki -30","Strong ki 0","Weak ki 15","Half ki 15"]
    assertEqual "Group Post Hl Lv2 " (action sixth) ["Strong ki -60","Strong ki 0","Weak ki 20","Half ki 20"]

testRally :: Test
testRally = TestCase $ do
    let weak = Character "Weak" "Friend" (ItemList 2 1 0 0 1 1) 10 100 10 30 60 []
        half = Character "Half" "Friend" (ItemList 2 1 0 0 1 1) 50 100 10 30 60 []
        (_, full) = getWarrior "Strong"
        first = performAction ["Rly","1", "Weak"] [full, weak]
        second = performAction ["Rly","2", "Weak"] [full, weak]
        third = performAction ["Rly","1", "Strong"] [weak, full]
        fourth = performAction ["Rly","3", "Weak"] [full, weak]
        fifth = performAction ["Rly","1", "A"] [full, weak, half]
        sixth = performAction ["Rly","2", "A"] [full, weak, half]
    assertEqual "Target Post Rly Lv 1" (action first) ["Strong stamina -10","Weak stamina 5"]
    assertEqual "Target Post Rly Lv 2" (action second) ["Strong stamina -25","Weak stamina 13"]
    assertEqual "Target Post Rly Lv 3" (action fourth) ["Strong stamina -40","Weak stamina 20"]
    assertEqual "Target Post Rly Lv 1 Overflow" (action third) ["Weak stamina -10","Strong stamina 0"]
    assertEqual "Group Post Rly Lv1 " (action fifth) ["Strong stamina -30","Strong stamina 0","Weak stamina 15","Half stamina 15"]
    assertEqual "Group Post Rly Lv2 " (action sixth) ["Strong stamina -60","Strong stamina 0","Weak stamina 30","Half stamina 30"]

testInvigorate :: Test
testInvigorate = TestCase $ do
    let (_, user) = getGod
        (_, target) = getWarrior "Target"
        first = performAction ["Invig","Target"] [user, target]
    assertEqual "Post Invig" (action first) ["God stamina -25","Targetinvigorate gained "]

testShield :: Test
testShield = TestCase $ do
    let (_, user) = getGod
        (_, target) = getWarrior "Target"
        first = performAction ["Shld","Target"] [user, target]
    assertEqual "Post Shield" (action first) ["God stamina -25","Targetshield gained "]

testAmp :: Test
testAmp = TestCase $ do
    let (_, user) = getGod
        (_, target) = getWarrior "Target"
        first = performAction ["Amp","Target"] [user, target]
    assertEqual "Post Amp" (action first) ["God ki -25","Targetamplify gained "]

testBarrier :: Test
testBarrier = TestCase $ do
    let (_, user) = getGod
        (_, target) = getWarrior "Target"
        first = performAction ["Brr","Target"] [user, target]
    assertEqual "Post Barrier" (action first) ["God ki -25","Targetbarrier gained "]

testDemoralize :: Test
testDemoralize = TestCase $ do
    let (_, user) = getGod
        (_, target) = getWarrior "Target"
        first = performAction ["Demor","Target"] [user, target]
    assertEqual "Post Demoralize" (action first) ["God stamina -25","Targetdemoralize gained "]

testDampen :: Test
testDampen = TestCase $ do
    let (_, user) = getGod
        (_, target) = getWarrior "Target"
        first = performAction ["Damp","Target"] [user, target]
    assertEqual "Post Dampen" (action first) ["God ki -25","Targetdampen gained "]

testIntimidate :: Test
testIntimidate = TestCase $ do
    let (_, user) = getGod
        (_, target) = getWarrior "Target"
        first = performAction ["Intim","Target"] [user, target]
    assertEqual "Post Intimidate" (action first) ["God stamina -25","Targetintimidate gained "]

testCurse :: Test
testCurse = TestCase $ do
    let (_, user) = getGod
        (_, target) = getWarrior "Target"
        first = performAction ["Crs","Target"] [user, target]
    assertEqual "Post Curse" (action first) ["God ki -25","Targetcurse gained "]

testAttacks :: Test
testAttacks = TestList
    [
        TestLabel "Stamina Attack Test" testStaminaAttack,
        TestLabel "Ki Attack Test" testKiAttack
    ]

testItems :: Test
testItems = TestList
    [
        TestLabel "Health Pot Test" testHealthPotion,
        TestLabel "Mana Pot Test" testManaPotion,
        TestLabel "Web Trap Test" testWebTrap,
        TestLabel "Throwing Knives Test" testThrowingKnives,
        TestLabel "Magical Seal Test" testMagicalSeal
    ]

testRecovery :: Test
testRecovery = TestList
    [
        TestLabel "Heal Test" testHeal,
        TestLabel "Rally Test" testRally
    ]

testBuffs :: Test
testBuffs = TestList
    [
        TestLabel "Invig Test" testInvigorate,
        TestLabel "Shield Test" testShield,
        TestLabel "Amp Test" testAmp,
        TestLabel "Barrier Test" testBarrier
    ]

testDebuffs :: Test
testDebuffs = TestList
    [
        TestLabel "Demoralize Test" testDemoralize,
        TestLabel "Damp Test" testDampen,
        TestLabel "Intimidate Test" testIntimidate,
        TestLabel "Curse Test" testCurse
    ]

{-
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

testHastePotion :: Test
testHastePotion = TestCase $ do
    let p1 = Character 100 100 30 50 20 []
        p2 = Character 100 100 30 50 20 ["hastePotion"]
        items = ItemList 2 2 2 2 2 2
    print $ useHastePotion p1 items

    assertEqual "Haste Potion Test" p1 p2

testWebTrap :: Test
testWebTrap = TestCase $ do
    let p1 = Character 100 100 30 50 20 []
        p2 = Character 100 100 30 50 20 ["webTrap"]
        items = ItemList 2 2 2 2 2 2
    print $ useWebTraps p1 items

    assertEqual "Web Trap Test" p1 p2

testThrowingKnives :: Test
testThrowingKnives = TestCase $ do
    let p1 = Character 80 100 50 50 20 []
        p2 = makeCharacter 100 50 20
        items = ItemList 2 2 2 2 2 2
    print $ useThrowingKnives p2 items

    assertEqual "Throwing Knives Test" p1 p2

testItems :: Test    
testItems = TestList [TestLabel "testHealthPotion" testHealthPotion, TestLabel "testManaPotion" testManaPotion, TestLabel "testHastePotion" testHastePotion, TestLabel "testWebTrap" testWebTrap, TestLabel "testThrowingKnives" testThrowingKnives]
-}