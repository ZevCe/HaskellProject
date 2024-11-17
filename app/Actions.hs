module Actions where

import Character

--refactor to use where instead of let in

--main attack functions, will only get more complex as time goes on and we start
--having to check for various status conditions before attacking as well
--keep trying to simplify in any way we can, very important to do so

--valid levels are 0-3
staminaSingleAttack :: Character -> Character -> Int -> (Character, Character)
staminaSingleAttack user target level =
    let
        atk = maxStamina user
        def = maxStamina target
        --if attack level is 0 then dont want positive amt of stamina used
        stamUsed = min (5 + (-15) * level) 0
        newUser = modifyStamina user stamUsed
        newTarget = modifyStamina target $ (atk `div` def) * (-10 + stamUsed)
    in (newUser, newTarget)

--valid levels are 0-3
kiSingleAttack :: Character -> Character -> Int -> (Character, Character)
kiSingleAttack user target level =
    let
        atk = maxKi user
        def = maxKi target
        --if attack level is 0 then dont want positive amt of stamina used
        kiUsed = min (5 + (-15) * level) 0
        newUser = modifyKi user kiUsed
        newTarget = modifyKi target $ (atk `div` def) * (-10 + kiUsed)
    in (newUser, newTarget)

--valid levels are 1-2
staminaGroupAttack :: Character -> [Character] -> Int -> (Character, [Character])
staminaGroupAttack user targets level =
    let
        newUser = modifyStamina user (- (30 * level))
        newTargets = [snd $ staminaSingleAttack user currentTarget (level - 1) | currentTarget <- targets ]
    in (newUser, newTargets)

--valid levels are 1-2
kiGroupAttack :: Character -> [Character] -> Int -> (Character, [Character])
kiGroupAttack user targets level =
    let
        newUser = modifyKi user (- (30 * level))
        newTargets = [snd $ kiSingleAttack user currentTarget (level - 1) | currentTarget <- targets ]
    in (newUser, newTargets)

--valid levels are 1-3
healingSingle :: Character -> Character -> Int -> (Character, Character)
healingSingle user target level =
    let
        newUser = modifyKi user (5 + (-15) * level)
        newTarget = modifyStamina target ( (-5) + 15 * level)
    in (newUser, newTarget)

--valid levels are 1-2
healingGroup :: Character -> [Character] -> Int -> (Character, [Character])
healingGroup user targets level =
    let
        newUser = modifyKi user (- (30 * level))
        newTargets = [snd $ healingSingle user currentTarget level | currentTarget <- targets]
    in (newUser, newTargets)

--doubles damage of next attack from target
invigorateTarget user target = undefined

invigorateGroup user targets = undefined

--causes target to take double damage on next incoming attack
demoralizeTarget user target = undefined

demoralizeGroup user targets = undefined

--reduces damage taken from next stamina attack
shield user target level = undefined

shieldGroup user target level = undefined

--reduces damage taken from next ki attack
barrier user target level = undefined

barrier user target level = undefined