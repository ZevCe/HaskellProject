module Actions where

import Character

--main attack functions, will only get more complex as time goes on and we start
--having to check for various status conditions before attacking as well
--keep trying to simplify in any way we can, very important to do so

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
        newTarget = modifyStamina buffedTarget $ (atk `div` def) * (-10 + stamUsed) * attackMods * defenseMods

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
        newTarget = modifyKi buffedTarget $ (atk `div` def) * (-10 + kiUsed) * attackMods * defenseMods

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
healingSingle :: Character -> Character -> Int -> (Character, Character)
healingSingle user target level =
    if ki newUser >= 0
        then (newUser, newTarget)
    else (user, target)
    where
        newUser = modifyKi user (5 + (-15) * level)
        amtHealed = min ((-5) + 15 * level) (maxStamina user - stamina user)
        newTarget = modifyStamina target amtHealed

--valid levels are 1-2
healingGroup :: Character -> [Character] -> Int -> (Character, [Character])
healingGroup user targets level =
    if ki newUser >= 0
        then (newUser, newTargets)
    else (user, targets)
    where
        newUser = modifyKi user (- (30 * level))
        newTargets = [snd $ healingSingle user currentTarget level | currentTarget <- targets]

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


--if extra time add status effects which cause next action to cost double/half