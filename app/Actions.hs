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
performAction :: [String] -> [Character] -> [Character]

--use health potion
performAction ("HeP":targetName:_) chars@(user:_) = useItem targetName chars restoreOperation usedItems
    where
        restoreOperation target = modifyStamina target $ min 20 $ maxStamina target - stamina target

        usedItems = ItemList
            (numHealthPotions (items user) - 1) (numManaPotions $ items user) (numThrowingKnives $ items user)
            (numMagicalSeals $ items user) (numWebTraps $ items user) (numHastePotions $ items user)

--use mana potion
performAction ("MP":targetName:_) chars@(user:_) = useItem targetName chars restoreOperation usedItems
    where 
        restoreOperation target = modifyKi target $ min 20 $ maxKi target - ki target
        
        usedItems = ItemList 
            (numHealthPotions $ items user) (numManaPotions (items user) - 1) (numThrowingKnives $ items user)
            (numMagicalSeals $ items user) (numWebTraps $ items user) (numHastePotions $ items user)


--use throwing knives
performAction ("TK":targetName:_) chars@(user:_) = useItem targetName chars damageOperation usedItems
    where
        damageOperation target = modifyStamina target (-20)
        
        usedItems = ItemList 
            (numHealthPotions $ items user) (numManaPotions $ items user) (numThrowingKnives (items user) - 1) 
            (numMagicalSeals $ items user) (numWebTraps $ items user) (numHastePotions $ items user)


--use magial seal
performAction ("MS":targetName:_) chars@(user:_) = useItem targetName chars damageOperation usedItems
    where
        damageOperation target = modifyKi target (-20)
        
        usedItems = ItemList 
            (numHealthPotions $ items user) (numManaPotions $ items user) (numThrowingKnives $ items user)
            (numMagicalSeals (items user) - 1) (numWebTraps $ items user) (numHastePotions $ items user)
        

--use web trap
performAction ("WT":targetName:_) chars@(user:_) = useItem targetName chars speedOperation usedItems
    where
        speedOperation target = modifyStatuses (modifySpeed target (-(speed target `div` 2))) ("webTrap" : statuses target)

        usedItems = ItemList 
            (numHealthPotions $ items user) (numManaPotions $ items user) (numThrowingKnives $ items user) 
            (numMagicalSeals $ items user) (numWebTraps (items user) - 1) (numHastePotions $ items user)

--use haste potion
performAction ("HaP":targetName:_) chars@(user:_) = useItem targetName chars speedOperation usedItems
    where
        speedOperation target = modifyStatuses (modifySpeed target $ speed target) ("hastePotion" : statuses target)

        usedItems = ItemList 
            (numHealthPotions $ items user) (numManaPotions $ items user) (numThrowingKnives $ items user) 
            (numMagicalSeals $ items user) (numWebTraps $ items user) (numHastePotions (items user) - 1)
        
-- --use stamina attack
-- performAction ("SA":level:targetName:_) chars = performSingleLevelAction (level:[targetName]) chars [0,1,2,3] stamAttack staminaSingleAttack

-- --use group stamina attack
-- --performAction ("SAA":level:_) chars = performGroupLevelAction level chars [1,2] enemyTeam stamAttack staminaGroupAttack

-- --use ki attack
-- performAction ("KA":level:targetName:_) chars = performSingleLevelAction (level:[targetName]) chars [0,1,2,3] kiAttack kiSingleAttack

-- --use group ki attack
-- --performAction ("KAA":level:_) chars = performGroupLevelAction level chars [1,2] enemyTeam kiAttack kiGroupAttack

-- --use heal
-- performAction ("Hl":level:targetName:_) chars = performSingleLevelAction (level:[targetName]) chars [1,2,3] heal healSingle

-- --use group heal
-- --performAction ("HlA":level:_) chars = performGroupLevelAction level chars [1,2] friendTeam heal healGroup

-- --use rally
-- performAction ("Rly":level:targetName:_) chars = performSingleLevelAction (level:[targetName]) chars [1,2,3] rally rallySingle

-- --use group rally
-- --performAction ("RlyA":level:_) chars = performGroupLevelAction level chars [1,2] friendTeam rally rallyGroup

-- --use invigorate
-- performAction ("Invig":targetName:_) chars = performStatusSingle targetName chars invigorate invigorateSingle

-- --use group invigorate
-- --performAction ("InvigA":_) (user:chars) = undefined

-- --use demoralize
-- performAction ("Demor":targetName:_) chars = performStatusSingle targetName chars demoralize demoralizeSingle

-- --use group demoralize
-- --performAction ("DemorA":_) (user:chars) = undefined

-- --use intimidate
-- performAction ("Intim":targetName:_) chars = performStatusSingle targetName chars intimidate intimidateSingle

-- --use group intimidate
-- --performAction ("IntimA":_) (user:chars) = undefined

-- --use shield
-- performAction ("Shld":targetName:_) chars = performStatusSingle targetName chars shield shieldSingle

-- --use group shield 
-- --performAction ("ShldA":_) (user:chars) = undefined

-- --use amplify
-- performAction ("Amp":targetName:_) chars = performStatusSingle targetName chars amplify amplifySingle

-- --use group amplify 
-- --performAction ("AmpA":_) (user:chars) = undefined

-- --use dampen
-- performAction ("Damp":targetName:_) chars = performStatusSingle targetName chars dampen dampenSingle

-- --use group dampen
-- --performAction ("DampA":_) (user:chars) = undefined

-- --use curse
-- performAction ("Crs":targetName:_) chars = performStatusSingle targetName chars curse curseSingle

-- --use group curse
-- --performAction ("CrsA":_) (user:chars) = undefined

-- --use barrier
-- performAction ("Brr":targetName:_) chars = performStatusSingle targetName chars barrier barrierSingle

-- --use group barrier
-- --performAction ("BrrA":_) (user:chars) = undefined

performAction _ _ = undefined

--generic function for using an item
useItem :: String -> [Character] -> (Character -> Character) -> ItemList -> [Character]
useItem targetName chars@(user:_) itemOperation newItemList = returnList
    where
        target = getTarget chars targetName
        newTarget = itemOperation target
        returnList =
            if target == user then updateCharList chars [modifyItems newTarget newItemList]
            else updateCharList chars [modifyItems user newItemList, newTarget]

useItem _ _ _ _  = undefined


-- --generic function for using single target actions which take a level
-- performSingleLevelAction :: [String] -> [Class] ->  [Int] -> (Class -> Bool) -> (Character -> Character -> Int -> (Character, Character)) -> Maybe [Class]
-- performSingleLevelAction (level:targetName:_) chars@(user:_) validLevels hasAction action =
--     if isNothing target || isNothing levelMaybe || fromJust levelMaybe `notElem` validLevels || not (hasAction user)
--         then Nothing
--     else Just returnList
--     where
--         target = getTarget chars targetName
--         justTarget = fromJust target
--         levelMaybe = readMaybe level :: Maybe Int
--         outcome = action (character user) (character justTarget) (fromJust levelMaybe)
--         returnList =
--             if justTarget == user
--                 then updateCharList chars [updateCharacter (updateCharacter user (fst outcome)) (snd outcome)]
--             else updateCharList chars [updateCharacter user (fst outcome), updateCharacter justTarget (snd outcome)]

-- performSingleLevelAction _ _ _ _ _ = undefined

-- --generic function for using group actions which take a level
-- --need to refactor character to also have name so we can match character to class
-- {-
-- performGroupLevelAction :: String -> [Class] -> [Int] -> ([Class] -> [Class]) -> 
--     (Class -> Bool) -> (Character -> [Character] -> Int -> (Character, [Character])) -> Maybe [Class]

-- performGroupLevelAction level chars@(user:_) validLevels filterTeam hasAction action = 
--     if isNothing levelMaybe || fromJust levelMaybe `notElem` validLevels || not (hasAction user) then Nothing
--     else Just returnList
--     where
--         levelMaybe = readMaybe level :: Maybe Int
--         targetChars = [character curChar | curChar <- filterTeam chars]
--         outcome = action (character user) targetChars (fromJust levelMaybe)
--         --if this doesnt work how I want it to may have to rethink large parts of program or cut group attacks for the time being
--         updatedTargets = [updateCharacter curClass curChar | curClass <- filterTeam chars, curChar <- snd outcome]
--         returnList = 
--             if user `elem` updatedTargets 
--                 then undefined
--             else updateCharacter user (fst outcome) : updatedTargets

-- performGroupLevelAction _ _ _ _ _ _ = undefined
-- -}

-- --generic function for using a single target status move
-- performStatusSingle :: String -> [Class] -> (Class -> Bool) -> (Character -> Character -> (Character, Character)) -> Maybe [Class]
-- performStatusSingle targetName chars@(user:_) hasAction action =
--     if isNothing target || not (hasAction user) then Nothing
--     else Just returnList
--     where
--         target = getTarget chars targetName
--         justTarget = fromJust target
--         outcome = action (character user) (character justTarget)
--         returnList =
--             if justTarget == user
--                 then updateCharList chars [updateCharacter (updateCharacter user (fst outcome)) (snd outcome)]
--             else updateCharList chars [updateCharacter user (fst outcome), updateCharacter justTarget (snd outcome)]

-- performStatusSingle _ _ _ _ = undefined

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

{-
--valid levels are 0-3
staminaSingleAttack :: Character -> Character -> Int -> (Character, Character)
staminaSingleAttack user target level =
    if stamina newUser >= 0
        then (newUser, newTarget)
    else (user, target)
    where
        atk = maxStamina user
        def = maxStamina target
        --if attack level is 0 then dont want positive amt of stamina used
        stamUsed = min (5 + (-15) * level) 0
        attackMods
          | "invigorate" `elem` statuses user && "demoralize" `elem` statuses user = 1
          | "invigorate" `elem` statuses user = 2
          | "demoralize" `elem` statuses user = 1 `div` 2
          | otherwise = 1
        defenseMods
          | "intimidate" `elem` statuses user && "shield" `elem` statuses user = 1
          | "intimidate" `elem` statuses user = 2
          | "shield" `elem` statuses user = 1 `div` 2
          | otherwise = 1
        buffedUser = modifyStatuses user (filter (\x -> x /= "invigorate" && x /= "demoralize") (statuses user))
        buffedTarget = modifyStatuses target (filter (\x -> x /= "intimidate" && x /= "shield") (statuses target))
        newUser = modifyStamina buffedUser stamUsed
        damageDealt = min (-1) $ (atk `div` def) * (-10 + stamUsed) * attackMods * defenseMods
        newTarget = modifyStamina buffedTarget damageDealt

--valid levels are 1-2
staminaGroupAttack :: Character -> [Character] -> Int -> (Character, [Character])
staminaGroupAttack user targets level =
    if stamina newUser >= 0
        then (newUser, newTargets)
    else (user, targets)
    where
        newUser = modifyStamina user (- (30 * level))
        newTargets = [snd $ staminaSingleAttack user currentTarget (level - 1) | currentTarget <- targets ]

--valid levels are 0-3
kiSingleAttack :: Character -> Character -> Int -> (Character, Character)
kiSingleAttack user target level =
    if ki newUser >= 0
        then (newUser, newTarget)
    else (user, target)
    where
        atk = maxKi user
        def = maxKi target
        --if attack level is 0 then dont want positive amt of stamina used
        kiUsed = min (5 + (-15) * level) 0
        attackMods
          | "amplify" `elem` statuses user && "dampen" `elem` statuses user = 1
          | "amplify" `elem` statuses user = 2
          | "dampen" `elem` statuses user = 1 `div` 2
          | otherwise = 1
        defenseMods
          | "curse" `elem` statuses user && "barrier" `elem` statuses user = 1
          | "curse" `elem` statuses user = 2
          | "barrier" `elem` statuses user = 1 `div` 2
          | otherwise = 1
        buffedUser = modifyStatuses user (filter (\x -> x /= "amplify" && x /= "dampen") (statuses user))
        buffedTarget = modifyStatuses target (filter (\x -> x /= "curse" && x /= "barrier") (statuses target))
        newUser = modifyKi buffedUser kiUsed
        damageDealt = min (-1) $ (atk `div` def) * (-10 + kiUsed) * attackMods * defenseMods
        newTarget = modifyKi buffedTarget damageDealt

--valid levels are 1-2
kiGroupAttack :: Character -> [Character] -> Int -> (Character, [Character])
kiGroupAttack user targets level =
    if ki newUser >= 0
        then (newUser, newTargets)
    else (user, targets)
    where
        newUser = modifyKi user (- (30 * level))
        newTargets = [snd $ kiSingleAttack user currentTarget (level - 1) | currentTarget <- targets ]

--valid levels are 1-3
healSingle :: Character -> Character -> Int -> (Character, Character)
healSingle user target level =
    if ki newUser >= 0
        then (newUser, newTarget)
    else (user, target)
    where
        newUser = modifyKi user (5 + (-15) * level)
        amtHealed = min ((-5) + 15 * level) (maxStamina user - stamina user)
        newTarget = modifyStamina target amtHealed

--valid levels are 1-2
healGroup :: Character -> [Character] -> Int -> (Character, [Character])
healGroup user targets level =
    if ki newUser >= 0
        then (newUser, newTargets)
    else (user, targets)
    where
        newUser = modifyKi user (- (30 * level))
        newTargets = [snd $ healSingle user currentTarget level | currentTarget <- targets]

--valid levels are 1-3
rallySingle :: Character -> Character -> Int -> (Character, Character)
rallySingle user target level =
    if stamina newUser >= 0
        then (newUser, newTarget)
    else (user, target)
    where
        newUser = modifyStamina user (5 + (-15) * level)
        amtRallied = min ((-5) + 15 * level) (maxKi user - ki user)
        newTarget = modifyKi target amtRallied

--valid levels are 1-2
rallyGroup :: Character -> [Character] -> Int -> (Character, [Character])
rallyGroup user targets level =
    if stamina newUser >= 0
        then (newUser, newTargets)
    else (user, targets)
    where
        newUser = modifyStamina user (- (30 * level))
        newTargets = [snd $ rallySingle user currentTarget level | currentTarget <- targets]

--helper functions for applying statuses
applyStaminaStatusSingle :: Character -> Character -> Int -> String -> (Character, Character)
applyStaminaStatusSingle user target cost status =
    if status `notElem` statuses target && stamina newUser >= 0
        then (newUser, newTarget)
    else (user, target)
    where
        newUser = modifyStamina user cost
        newTarget = modifyStatuses target (status : statuses target)

applyStaminaStatusGroup :: Character -> [Character] -> Int -> String -> (Character, [Character])
applyStaminaStatusGroup user targets cost status =
    if stamina newUser >= 0
        then (newUser, newTargets)
    else (user, targets)
    where
        newUser = modifyStamina user cost
        newTargets = [snd $ applyStaminaStatusSingle user currentTarget 0 status | currentTarget <- targets]

applyKiStatusSingle :: Character -> Character -> Int -> String -> (Character, Character)
applyKiStatusSingle user target cost status =
    if status `notElem` statuses target && ki newUser >= 0
        then (newUser, newTarget)
    else (user, target)
    where
        newUser = modifyKi user cost
        newTarget = modifyStatuses target (status : statuses target)

applyKiStatusGroup :: Character -> [Character] -> Int -> String -> (Character, [Character])
applyKiStatusGroup user targets cost status =
    if ki newUser >= 0
        then (newUser, newTargets)
    else (user, targets)
    where
        newUser = modifyKi user cost
        newTargets = [snd $ applyKiStatusSingle user currentTarget 0 status | currentTarget <- targets]

--target deals double damage on next stamina attack
invigorateSingle :: Character -> Character -> (Character, Character)
invigorateSingle user target = applyStaminaStatusSingle user target (-25) "invigorate"

invigorateGroup :: Character -> [Character] -> (Character, [Character])
invigorateGroup user targets = applyStaminaStatusGroup user targets (-60) "invigorate"

--target deals half damage on next stamina attack
demoralizeSingle :: Character -> Character -> (Character, Character)
demoralizeSingle user target = applyStaminaStatusSingle user target (-25) "demoralize"

demoralizeGroup :: Character -> [Character] -> (Character, [Character])
demoralizeGroup user targets = applyStaminaStatusGroup user targets (-60) "demoralize"

--target takes double damage from next stamina attack
intimidateSingle :: Character -> Character -> (Character, Character)
intimidateSingle user target = applyStaminaStatusSingle user target (-25) "intimidate"

intimidateGroup :: Character -> [Character] -> (Character, [Character])
intimidateGroup user targets = applyStaminaStatusGroup user targets (-60) "intimidate"

--target takes half damage from next stamina attack
shieldSingle :: Character -> Character -> (Character, Character)
shieldSingle user target = applyStaminaStatusSingle user target (-25) "shield"

shieldGroup :: Character -> [Character] -> (Character, [Character])
shieldGroup user targets = applyStaminaStatusGroup user targets (-60) "shield"

--target deals double damage on next ki attack
amplifySingle :: Character -> Character -> (Character, Character)
amplifySingle user target = applyKiStatusSingle user target (-25) "amplify"

amplifyGroup :: Character -> [Character] -> (Character, [Character])
amplifyGroup user target = applyKiStatusGroup user target (-60) "amplify"

--target deals half damage on next ki attack
dampenSingle :: Character -> Character -> (Character, Character)
dampenSingle user target = applyKiStatusSingle user target (-25) "dampen"

dampenGroup :: Character -> [Character] -> (Character, [Character])
dampenGroup user target = applyKiStatusGroup user target (-60) "dampen"

--target takes double damage from next ki attack
curseSingle :: Character -> Character -> (Character, Character)
curseSingle user target = applyKiStatusSingle user target (-25) "curse"

curseGroup :: Character -> [Character] -> (Character, [Character])
curseGroup user targets = applyKiStatusGroup user targets (-60) "curse"

--target takes half damage from next ki attack
barrierSingle :: Character -> Character -> (Character, Character)
barrierSingle user target = applyKiStatusSingle user target (-25) "barrier"

barrierGroup :: Character -> [Character] -> (Character, [Character])
barrierGroup user target = applyKiStatusGroup user target (-60) "barrier"
-}