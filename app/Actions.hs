module Actions where
--somebody smarter than me should do the thing where you only export certain functions
--and the only function we need to export from this file is performAction

import System.Random
import Character

--takes a list of strings and a list of all characters, pattern matches the string list
--to determine which actions to perform and then returns an updated list of characters
--will eventually instead return a log keeping track of which actions happened throughout
--Assumptions:
--The first character in the list of characters is the one performing the action
--The action specified by the list of strings is valid
--The character can perform the given action (i.e. has enough items/ki/stamina or the ability to use action in the first place)
--no invalid inputs (all levels are correct levels, all comand names pattern match correctly etc.)
performAction :: [String] -> [Character] -> TurnPacket

--health potion (restores up to 20 stamina)
performAction ("HeP":targetName:_) chars@(user:_) = useItem [" health potion ", targetName] chars modifyTarget usedItems
    where
        modifyTarget target = ([logEntry], newChar)
            where
                amtHealed = min 20 $ maxStamina target - stamina target
                logEntry = name target ++ " stamina +" ++ show amtHealed
                newChar = modifyStamina target amtHealed

        usedItems = ItemList
            (numHealthPotions (items user) - 1) (numManaPotions $ items user) (numThrowingKnives $ items user)
            (numMagicalSeals $ items user) (numWebTraps $ items user) (numHastePotions $ items user)

--mana potion (restores up to 20 ki)
performAction ("MP":targetName:_) chars@(user:_) = useItem [" mana potion ", targetName] chars modifyTarget usedItems
    where
        modifyTarget target = ([logEntry], newChar)
            where
                amtHealed = min 20 $ maxKi target - ki target
                logEntry = name target ++ " ki +" ++ show amtHealed
                newChar = modifyKi target amtHealed

        usedItems = ItemList
            (numHealthPotions $ items user) (numManaPotions (items user) - 1) (numThrowingKnives $ items user)
            (numMagicalSeals $ items user) (numWebTraps $ items user) (numHastePotions $ items user)

--throwing knives (deals 20 stamina damage)
performAction ("TK":targetName:_) chars@(user:_) = useItem [" throwing knives ", targetName] chars modifyTarget usedItems
    where
        modifyTarget target = (logEntry, newChar)
            where
                logEntry
                 | stamina target <= 20 = [name target ++ " stamina -20", name target ++ " perished"]
                 | otherwise = [name target ++ " stamina -20"]

                newChar = modifyStamina target (-20)

        usedItems = ItemList
            (numHealthPotions $ items user) (numManaPotions $ items user) (numThrowingKnives (items user) - 1)
            (numMagicalSeals $ items user) (numWebTraps $ items user) (numHastePotions $ items user)

--use magial seal (deals 20 ki damage)
performAction ("MS":targetName:_) chars@(user:_) = useItem [" magical seals ", targetName] chars modifyTarget usedItems
    where
        modifyTarget target = (logEntry, newChar)
            where
                logEntry
                 | ki target <= 20 = [name target ++ " ki -20", name target ++ " perished"]
                 | otherwise = [name target ++ " ki -20"]

                newChar = modifyKi target (-20)

        usedItems = ItemList
            (numHealthPotions $ items user) (numManaPotions $ items user) (numThrowingKnives $ items user)
            (numMagicalSeals (items user) - 1) (numWebTraps $ items user) (numHastePotions $ items user)

--use web trap (halves target's speed)
performAction ("WT":targetName:_) chars@(user:_) = useItem [" web trap ", targetName] chars modifyTarget usedItems
    where
        modifyTarget target = (logEntry, newChar)
            where
                spdLost = -(speed target `div` 2)
                logEntry = [name target ++ " web trapped (speed halved)", name target ++ " speed -" ++ show spdLost]
                slowedChar = modifySpeed target spdLost
                newChar = addStatusWithEffect target "webTrap" slowedChar


        usedItems = ItemList
            (numHealthPotions $ items user) (numManaPotions $ items user) (numThrowingKnives $ items user)
            (numMagicalSeals $ items user) (numWebTraps (items user) - 1) (numHastePotions $ items user)

--use haste potion (doubles target's speed)
performAction ("HaP":targetName:_) chars@(user:_) = useItem [" haste potion ", targetName] chars modifyTarget usedItems
    where
        modifyTarget target = (logEntry, newChar)
            where
                spdGained = speed target
                logEntry = [name target ++ " hasted (speed doubled)", name target ++ " speed +" ++ show spdGained]
                hastedChar = modifySpeed target spdGained
                newChar = addStatusWithEffect target "hastePotion" hastedChar


        usedItems = ItemList
            (numHealthPotions $ items user) (numManaPotions $ items user) (numThrowingKnives $ items user)
            (numMagicalSeals $ items user) (numWebTraps $ items user) (numHastePotions (items user) - 1)

--stamina attack
--single target attacks can have levels 0-3
--group attacks can have levels 1-2
performAction ("SA":level:targetName:_) chars = attackTarget [" stamina ", level, targetName] chars enemyTeam modifyStamina maxStamina userStatuses targetStatuses
    where
        userStatuses = ("invigorate", "demoralize")
        targetStatuses = ("intimidate","shield")

--ki attack
--single target attacks can have levels 0-3
--group attacks can have levels 1-2
performAction ("KA":level:targetName:_) chars = attackTarget [" ki ", level, targetName] chars enemyTeam modifyKi maxKi userStatuses targetStatuses
    where
        userStatuses = ("amplify", "dampen")
        targetStatuses = ("curse", "barrier")

--heal
--single target restores can have levels 1-3
--group restores can have levels 1-2
performAction ("Hl":level:targetName:_) chars = restoreTarget [" ki ", level, targetName] chars friendTeam modifyKi maxKi ki

--rally
--single target restores can have levels 1-3
--group restores can have levels 1-2
performAction ("Rly":level:targetName:_) chars = restoreTarget [" stamina ", level, targetName] chars friendTeam modifyStamina maxStamina stamina

--invigorate (target deals double damage on next stamina attack)
performAction ("Invig":targetName:_) chars = statusTarget [" stamina ", "invigorate", targetName] chars friendTeam modifyStamina

--demoralize (deals half damage on next stamina attack)
performAction ("Demor":targetName:_) chars = statusTarget [" stamina ", "demoralize", targetName] chars enemyTeam modifyStamina 

--intimidate (takes double damage from next stamina attack)
performAction ("Intim":targetName:_) chars = statusTarget [" stamina ", "intimidate", targetName] chars enemyTeam modifyStamina

--shield (takes half damage from next stamina attack)
performAction ("Shld":targetName:_) chars = statusTarget [" stamina ", "shield", targetName] chars friendTeam modifyStamina 

--amplify (deals double damage on next ki attack)
performAction ("Amp":targetName:_) chars = statusTarget [" ki ", " amplify", targetName] chars friendTeam modifyKi 

--dampen (deals half damage on next ki attack)
performAction ("Damp":targetName:_) chars = statusTarget [" ki ", " dampen", targetName] chars enemyTeam modifyKi 

--curse (takes double damage from next ki attack)
performAction ("Crs":targetName:_) chars = statusTarget [" ki ", " curse", targetName] chars enemyTeam modifyKi 

--barrier (takes half damage from next ki attack)
performAction ("Brr":targetName:_) chars = statusTarget [" ki ", " barrier", targetName] chars friendTeam modifyKi 

--determine enemies move, will impliment once we have more front end
performAction ("Ea":moves) chars = 
    case moveType of
        "KA" -> attackTarget [" ki ", levelType, targetEnemy] chars friendTeam modifyKi maxKi ("amplify", "dampen") ("curse", "barrier")
        "Invig" -> statusTarget [" stamina ", " invigorate", targetAlly] chars enemyTeam modifyStamina
        "Demor" -> statusTarget [" stamina ", " demoralize", targetEnemy] chars friendTeam modifyStamina 
        "Intim" -> statusTarget [" stamina ", " intimidate", targetEnemy] chars friendTeam modifyStamina
        "Shld" -> statusTarget [" stamina ", " shield", targetAlly] chars enemyTeam modifyStamina 
        "Amp" -> statusTarget [" ki ", " amplify", targetAlly] chars enemyTeam modifyKi 
        "Damp" -> statusTarget [" ki ", " dampen", targetEnemy] chars friendTeam modifyKi 
        "Crs" -> statusTarget [" ki ", " curse", targetEnemy] chars friendTeam modifyKi 
        "Brr" -> statusTarget [" ki ", " barrier", targetAlly] chars enemyTeam modifyKi 
        _ -> attackTarget [" stamina ", levelType, targetEnemy] chars friendTeam modifyStamina maxStamina ("invigorate", "demoralize") ("intimidate","shield")
    where
        seed = mkStdGen ((length ((friendTeam chars))-1) * (length ((friendTeam chars))) + (length (enemyTeam chars)) * 2)
        (enemyIndex, afterEnemyGen) = randomInt (0, (length (friendTeam chars))-1) seed
        (allyIndex, afterAllyGen) = randomInt (0, (length (enemyTeam chars))-1) afterEnemyGen
        (moveIndex, afterMoveGen) = randomInt (0, (length moves)-1) afterAllyGen
        (levelIndex, afterLevelGen) = randomInt (0, 3) afterMoveGen
        moveType = moves !! moveIndex
        levelType = show levelIndex
        targetEnemy = (name ((friendTeam chars) !! enemyIndex))
        targetAlly = (name ((enemyTeam chars) !! allyIndex))

performAction _ _ = undefined

randomInt :: (Int, Int) -> StdGen -> (Int, StdGen)
randomInt range gen = randomR range gen


--generic function for using an item
useItem :: [String] -> [Character] -> (Character -> ([String], Character)) -> ItemList -> TurnPacket
useItem (itemName:targetName:_) chars@(user:_) modifyTarget newItemList = returnUpdatedChars ([itemName], modifiedUser) [target] modifyTarget chars
    where
        modifiedUser = modifyItems user newItemList

        target = getTarget chars targetName

useItem _ _ _ _  = undefined

--generic attack function for attacks
attackTarget :: [String] -> [Character] -> ([Character] -> [Character]) ->  (Character -> Int -> Character) -> (Character -> Int) -> (String, String) -> (String, String) -> TurnPacket
attackTarget (atkType:levelString:targetName:_) chars@(user:_) getTeam modifyStat maxStat userStatuses targetStatuses = returnUpdatedChars modifiedUser targets modifyTarget chars
    where
        level = read levelString :: Int

        cost
         | targetName == "A" = - (30 * level)
         | otherwise = min (5 + (-15) * level) 0

        damageCalc t = min (-1) $ (maxStat user `div` maxStat t) * costMod * attackMod * defenseMod
            where
                costMod
                 | targetName == "A" = cost `div` 2
                 | otherwise = -10 + cost

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

        --helper function for adding a used buff to the log
        logBuff :: ((String, String) -> String) -> (String, String) -> Character -> [String]
        logBuff select buffs char
            | select buffs `elem` statuses char = [name char ++ fst buffs ++ " applied"]
            | otherwise = []

        modifiedUser = (logEntries, newUser)
            where
                logEntries = [name user ++ atkType ++ show cost] ++ fstBuffLog ++ sndBuffLog
                    where
                        fstBuffLog = logBuff fst userStatuses user
                        sndBuffLog = logBuff snd userStatuses user
                newUser = modifyStat buffedUser cost
                buffedUser = removeStatuses user [fst userStatuses, snd userStatuses]

        targets
         | targetName == "A" = getTeam chars
         | otherwise = [getTarget chars targetName]

        modifyTarget t = (logEntries, newTarget)
            where
                buffedTarget = removeStatuses t [fst userStatuses, snd targetStatuses]
                newTarget = modifyStat buffedTarget $ damageCalc t

                logEntries = [name t ++ atkType ++ show (damageCalc t)] ++ hasDied ++ fstBuffLog ++ sndBuffLog
                    where
                        hasDied
                         | stamina newTarget <= 0 || ki newTarget <= 0 = [name t ++ " has perished"] 
                         | otherwise = []
                        fstBuffLog = logBuff fst targetStatuses t
                        sndBuffLog = logBuff snd targetStatuses t

attackTarget _ _ _ _ _ _ _ = undefined

--generic function for restore abilities
restoreTarget :: [String] -> [Character] -> ([Character] -> [Character]) ->  (Character -> Int -> Character) -> (Character -> Int) -> (Character -> Int) -> TurnPacket
restoreTarget (rstrType:levelString:targetName:_) chars@(user:_) getTeam modifyStat maxStat stat = returnUpdatedChars modifiedUser targets modifyTarget chars
    where
        level = read levelString :: Int

        cost
         | targetName == "A" = - (30 * level)
         | otherwise = 5 + (-15) * level

        modifiedUser = (logEntries, newUser) 
            where
                logEntries = [name user ++ rstrType ++ show cost]
                newUser = modifyStat user cost

        targets
         | targetName == "A" = getTeam chars
         | otherwise = [getTarget chars targetName]

        modifyTarget t = (logEntries, newUser)
            where
                rstrAmt = min (- (cost `div` 2)) (maxStat t - stat t)
                logEntries = [name t ++ rstrType ++ show rstrAmt]
                newUser = modifyStat t rstrAmt

restoreTarget _ _ _ _ _ _ = undefined

--generic function for status abilities
statusTarget :: [String] -> [Character] -> ([Character] -> [Character]) -> (Character -> Int -> Character)  -> TurnPacket
statusTarget (costType:status:targetName:_) chars@(user:_) getTeam modifyStat = returnUpdatedChars modifiedUser targets modifyTarget chars
    where
        modifiedUser = (logEntry, newUser) 
            where
                cost 
                 | targetName == "A" = -60 --cost of buff/debuff is always same
                 | otherwise = -25

                logEntry = [name user ++ costType ++ show cost]

                newUser = modifyStat user cost
 
        targets
         | targetName == "A" = getTeam chars
         | otherwise = [getTarget chars targetName]

        modifyTarget t = (logEntry, newTarget)
            where
                logEntry = [name t ++ status ++ " gained "]
                newTarget = addStatus t status

statusTarget _ _ _ _ = undefined

--helper function for returning results from our generic functions
returnUpdatedChars :: ([String], Character) -> [Character] -> (Character -> ([String], Character)) -> [Character] -> TurnPacket
returnUpdatedChars modifiedUser targets modifyTarget chars = TurnPacket logResult (fixTurnOrder (snd modifiedUser) result)
    where
        --after we re-organize everyone by speed we need to re-iterate back through
        --whose turn it currently is
        fixTurnOrder :: Character -> [Character] -> [Character]
        fixTurnOrder turnChar (char:chars') =
            if turnChar == char then chars' ++ [char]
            else fixTurnOrder turnChar (chars' ++ [char])
        fixTurnOrder _ _ = undefined


        logResult :: [String]
        logResult = fst modifiedUser ++ processTargets targets
            where
                processTargets [] = []
                processTargets (t:ts) = fst (modifyTarget t) ++ processTargets ts

        --result is the result of applying our modifyTarget function onto all of our targets 
        result :: [Character]
        result
         | snd modifiedUser `elem` targets = replaceChars chars (snd (modifyTarget $ snd modifiedUser) : [snd $ modifyTarget curTar | curTar <- reducedTargets])
         | otherwise = replaceChars chars (snd modifiedUser : [ snd $ modifyTarget curTar | curTar <- targets])
         where
            reducedTargets :: [Character]
            reducedTargets = filter (/= snd modifiedUser) targets

            --replace characters from the old list with characters from the new list
            --filtering out all the dead characters then re-organizing them into turn order
            replaceChars :: [Character] -> [Character] -> [Character]
            replaceChars oldChars updatedChars = getTurnOrder aliveChars
                where
                    unaffectedChars = filter (`notElem` updatedChars) oldChars
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