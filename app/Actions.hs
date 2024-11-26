module Actions where
--somebody smarter than me should do the thing where you only export certain functions
--and the only function we need to export from this file is performAction


import Character

--takes a list of strings and a list of all characters, pattern matches the string list
--to determine which actions to perform and then returns an updated list of characters
--will eventually instead return a log keeping track of which actions happened throughout
--Assumptions:
--The first character in the list of characters is the one performing the action
--The action specified by the list of strings is valid
--The character can perform the given action (i.e. has enough items/ki/stamina or the ability to use action in the first place)
--no invalid inputs (all levels are correct levels, all comand names pattern match correctly etc.)
performAction :: [String] -> [Character] -> [Character]

--use health potion
performAction ("HeP":targetName:_) chars@(user:_) = useItem targetName chars modifyTarget usedItems
    where
        modifyTarget target = modifyStamina target $ min 20 $ maxStamina target - stamina target

        usedItems = ItemList
            (numHealthPotions (items user) - 1) (numManaPotions $ items user) (numThrowingKnives $ items user)
            (numMagicalSeals $ items user) (numWebTraps $ items user) (numHastePotions $ items user)

--use mana potion
performAction ("MP":targetName:_) chars@(user:_) = useItem targetName chars modifyTarget usedItems
    where
        modifyTarget target = modifyKi target $ min 20 $ maxKi target - ki target

        usedItems = ItemList
            (numHealthPotions $ items user) (numManaPotions (items user) - 1) (numThrowingKnives $ items user)
            (numMagicalSeals $ items user) (numWebTraps $ items user) (numHastePotions $ items user)

--use throwing knives
performAction ("TK":targetName:_) chars@(user:_) = useItem targetName chars modifyTarget usedItems
    where
        modifyTarget target = modifyStamina target (-20)

        usedItems = ItemList
            (numHealthPotions $ items user) (numManaPotions $ items user) (numThrowingKnives (items user) - 1)
            (numMagicalSeals $ items user) (numWebTraps $ items user) (numHastePotions $ items user)

--use magial seal
performAction ("MS":targetName:_) chars@(user:_) = useItem targetName chars modifyTarget usedItems
    where
        modifyTarget target = modifyKi target (-20)

        usedItems = ItemList
            (numHealthPotions $ items user) (numManaPotions $ items user) (numThrowingKnives $ items user)
            (numMagicalSeals (items user) - 1) (numWebTraps $ items user) (numHastePotions $ items user)

--use web trap
performAction ("WT":targetName:_) chars@(user:_) = useItem targetName chars modifyTarget usedItems
    where
        modifyTarget target = addStatusWithEffect target "webTrap" slowedTarget
            where slowedTarget = modifySpeed target (-(speed target `div` 2))

        usedItems = ItemList
            (numHealthPotions $ items user) (numManaPotions $ items user) (numThrowingKnives $ items user)
            (numMagicalSeals $ items user) (numWebTraps (items user) - 1) (numHastePotions $ items user)

--use haste potion
performAction ("HaP":targetName:_) chars@(user:_) = useItem targetName chars modifyTarget usedItems
    where
        modifyTarget target = addStatusWithEffect target "hastePotion" hastedTarget
            where hastedTarget = modifySpeed target $ speed target

        usedItems = ItemList
            (numHealthPotions $ items user) (numManaPotions $ items user) (numThrowingKnives $ items user)
            (numMagicalSeals $ items user) (numWebTraps $ items user) (numHastePotions (items user) - 1)

--use stamina attack
performAction ("SA":level:targetName:_) chars = attackTarget [level, targetName] chars modifyStamina maxStamina userStatuses targetStatuses
    where
        userStatuses = ("invigorate", "demoralize")
        targetStatuses = ("intimidate","shield")

--use ki attack
performAction ("KA":level:targetName:_) chars = attackTarget [level, targetName] chars modifyKi maxKi userStatuses targetStatuses
    where
        userStatuses = ("amplify", "dampen")
        targetStatuses = ("curse", "barrier")

--use heal
performAction ("Hl":level:targetName:_) chars = restoreTarget [level, targetName] chars modifyKi maxKi ki

--use rally
performAction ("Rly":level:targetName:_) chars = restoreTarget [level, targetName] chars modifyStamina maxStamina stamina

--use invigorate (target deals double damage on next stamina attack)
performAction ("Invig":targetName:_) chars = statusTarget targetName chars modifyStamina "invigorate"

--use demoralize (deals half damage on next stamina attack)
performAction ("Demor":targetName:_) chars = statusTarget targetName chars modifyStamina "demoralize"

--use intimidate (takes double damage from next stamina attack)
performAction ("Intim":targetName:_) chars = statusTarget targetName chars modifyStamina "intimidate"

--use shield (takes half damage from next stamina attack)
performAction ("Shld":targetName:_) chars = statusTarget targetName chars modifyStamina "shield"

--use amplify (deals double damage on next ki attack)
performAction ("Amp":targetName:_) chars = statusTarget targetName chars modifyKi "amplify"

--use dampen (deals half damage on next ki attack)
performAction ("Damp":targetName:_) chars = statusTarget targetName chars modifyKi "dampen"

--use curse (takes double damage from next ki attack)
performAction ("Crs":targetName:_) chars = statusTarget targetName chars modifyKi "curse"

--use barrier (takes half damage from next ki attack)
performAction ("Brr":targetName:_) chars = statusTarget targetName chars modifyKi "barrier"

performAction _ _ = undefined

--generic function for using an item
useItem :: String -> [Character] -> (Character -> Character) -> ItemList -> [Character]
useItem targetName chars@(user:_) modifyTarget newItemList = returnUpdatedChars modifiedUser [target] modifyTarget chars
    where
        modifiedUser = modifyItems user newItemList

        target = getTarget chars targetName

useItem _ _ _ _  = undefined

--helper function for determing whether we should use attackSingle or attackGroup based off of user input
attackTarget :: [String] -> [Character] -> (Character -> Int -> Character) -> (Character -> Int) -> (String, String) -> (String, String) -> [Character]
attackTarget (level:targetName:_)
         | targetName == "A" = attackGroup level
         | otherwise = attackSingle [level, targetName]
attackTarget _ = undefined

--generic function for using single target attacks
--levels 0-3
attackSingle :: [String] -> [Character] -> (Character -> Int -> Character) -> (Character -> Int) -> (String, String) -> (String, String) -> [Character]
attackSingle (levelString:targetName:_) chars@(user:_) modifyStat maxStat userStatuses targetStatuses = returnUpdatedChars modifiedUser [target] modifyTarget chars
    where
        level = read levelString :: Int

        cost = min (5 + (-15) * level) 0

        damageCalc t = min (-1) $ (maxStat user `div` maxStat t) * (-10 + cost) * attackMod * defenseMod
            where
                attackMod
                    | fst userStatuses `elem` statuses user && snd userStatuses `elem` statuses user = 1
                    | fst userStatuses `elem` statuses user = 2
                    | snd userStatuses `elem` statuses user = 1 `div` 2
                    | otherwise = 1

                defenseMod
                    | fst targetStatuses `elem` statuses t && snd targetStatuses `elem` statuses t = 1
                    | fst targetStatuses `elem` statuses t = 2
                    | snd targetStatuses `elem` statuses t = 1 `div` 2
                    | otherwise = 1

        modifiedUser = modifyStat buffedUser cost
            where buffedUser = removeStatuses user [fst userStatuses, snd userStatuses]

        target = getTarget chars targetName

        modifyTarget t = modifyStat buffedTarget $ damageCalc t
            where buffedTarget = removeStatuses t [fst userStatuses, snd targetStatuses]

attackSingle _ _ _ _ _ _ = undefined

--generic function for group attacks
--valid levels are 1-2
attackGroup :: String -> [Character] -> (Character -> Int -> Character) -> (Character -> Int) -> (String, String) -> (String, String) -> [Character]
attackGroup levelString chars@(user:_) modifyStat maxStat userStatuses targetStatuses = returnUpdatedChars modifiedUser targets modifyTarget chars
    where
        level = read levelString :: Int

        cost = - (30 * level)

        damageCalc t = min (-1) $ (maxStat user `div` maxStat t) * (cost `div` 2) * attackMod * defenseMod
            where
                attackMod
                    | fst userStatuses `elem` statuses user && snd userStatuses `elem` statuses user = 1
                    | fst userStatuses `elem` statuses user = 2
                    | snd userStatuses `elem` statuses user = 1 `div` 2
                    | otherwise = 1

                defenseMod
                    | fst targetStatuses `elem` statuses t && snd targetStatuses `elem` statuses t = 1
                    | fst targetStatuses `elem` statuses t = 2
                    | snd targetStatuses `elem` statuses t = 1 `div` 2
                    | otherwise = 1

        modifiedUser = modifyStat buffedUser cost
            where buffedUser = removeStatuses user [fst userStatuses, snd userStatuses]

        targets = enemyTeam chars

        modifyTarget t = modifyStat buffedTarget $ damageCalc t
            where buffedTarget = removeStatuses t [fst userStatuses, snd targetStatuses]

attackGroup _ _ _ _ _ _ = undefined

--helper function for helping to determine whether we should use restore single or restore group based off of user input
restoreTarget :: [String] -> [Character] -> (Character -> Int -> Character) -> (Character -> Int) -> (Character -> Int) -> [Character]
restoreTarget (level:targetName:_)
         | targetName == "A" = restoreGroup level
         | otherwise = restoreSingle (level:[targetName])

restoreTarget _ = undefined

--generic function for single target restore spells
--levels 1-3
restoreSingle :: [String] -> [Character] -> (Character -> Int -> Character) -> (Character -> Int) -> (Character -> Int) -> [Character]
restoreSingle (levelString:targetName:_) chars@(user:_) modifyStat maxStat stat = returnUpdatedChars modifiedUser [target] modifyTarget chars
    where
        level = read levelString :: Int

        cost = 5 + (-15) * level

        --if we want user to restore different stat from spell cost stat then we need to pass in two modifyStats
        modifiedUser = modifyStat user cost

        target = getTarget chars targetName

        modifyTarget t = modifyStat t $ min (- (cost `div` 2)) (maxStat t - stat t)

restoreSingle _ _ _ _ _ = undefined

--generic function for group restore spells
restoreGroup :: String -> [Character] -> (Character -> Int -> Character) -> (Character -> Int) -> (Character -> Int) -> [Character]
restoreGroup levelString chars@(user:_) modifyStat maxStat stat = returnUpdatedChars modifiedUser targets modifyTarget chars
    where
        level = read levelString :: Int

        cost = - (30 * level)

        modifiedUser = modifyStat user cost

        targets = friendTeam chars

        modifyTarget t = modifyStat t $ min (- (cost `div` 2)) (maxStat t - stat t)

restoreGroup _ _ _ _ _ = undefined

--helper function for determing whether we should use statusSingle or statusGroup based off of user input
statusTarget :: String -> [Character] -> (Character -> Int -> Character) -> String -> [Character]
statusTarget targetName
         | targetName == "A" = statusGroup friendTeam
         | otherwise = statusSingle targetName

--generic function for using a single target status move
statusSingle :: String -> [Character] -> (Character -> Int -> Character) -> String -> [Character]
statusSingle targetName chars@(user:_) modifyStat status = returnUpdatedChars modifiedUser [target] modifyTarget chars
    where
        modifiedUser = modifyStat user (-25) --cost of buff/debuff is always same

        target = getTarget chars targetName

        modifyTarget t = addStatus t status

statusSingle _ _ _ _ = undefined

--generic function for using a group target status move
statusGroup :: ([Character] -> [Character]) -> [Character] -> (Character -> Int -> Character) -> String -> [Character]
statusGroup getTeam chars@(user:_) modifyStat status = returnUpdatedChars modifiedUser targets modifyTarget chars
    where
        modifiedUser = modifyStat user (-60) --cost of buff/debuff is always same

        targets = getTeam chars

        modifyTarget t = addStatus t status

statusGroup _ _ _ _ = undefined

--helper function for returning results from our group target generic functions
returnUpdatedChars :: Character -> [Character] -> (Character -> Character) -> [Character] -> [Character]
returnUpdatedChars modifiedUser targets modifyTarget chars
     | modifiedUser `elem` targets = replaceChars chars (modifyTarget modifiedUser : [modifyTarget curTar | curTar <- reducedTargets])
     | otherwise = replaceChars chars (modifiedUser : [modifyTarget curTar | curTar <- targets])
    where
        reducedTargets = filter (/= modifiedUser) targets

        --replace characters from the old list with characters from the new list
        --filtering out all the dead characters then re-organizing them into turn order
        replaceChars :: [Character] -> [Character] -> [Character]
        replaceChars oldList updatedChars = getTurnOrder aliveChars
            where
                unaffectedChars = filter (`notElem` updatedChars) oldList
                aliveChars = filter (\c -> ki  c > 0 && stamina c > 0) (updatedChars ++ unaffectedChars)

-- enemyActions :: [Class] -> IO [Class]
-- enemyActions chars@(turnChar:_) = do
--     if stamAttack turnChar then return $ fixTurnOrder turnChar $ enemyAttack chars stamina staminaSingleAttack
--     else return $ fixTurnOrder turnChar $ enemyAttack chars ki kiSingleAttack

-- enemyActions _ = undefined

-- enemyAttack :: [Class] -> (Character -> Int) -> (Character -> Character -> Int -> (Character, Character)) -> [Class]
-- enemyAttack chars@(turnChar:_) stat attack = returnList 
--     where 
--         potentialTargets = friendTeam chars
--         bestTarget = minimumBy (\c1 c2 -> compare (stat (character c1)) (stat (character c2))) potentialTargets
--         outcome = attack (character turnChar) (character bestTarget) 0
--         returnList =
--             if turnChar == bestTarget
--                 then updateCharList chars [updateCharacter (updateCharacter turnChar (fst outcome)) (snd outcome)]
--             else updateCharList chars [updateCharacter turnChar (fst outcome), updateCharacter bestTarget (snd outcome)]

-- enemyAttack _ _ _ = undefined