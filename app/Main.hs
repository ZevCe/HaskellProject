{-# LANGUAGE OverloadedStrings #-}
module Main where
import Character
import Class
import Encounter
import Item
import Actions
import Data.List
import Data.Maybe
import Control.Concurrent
import Text.Read (readMaybe)

{-
import Web.Scotty

main :: IO ()
main = scotty 3000 $
    get "/:word" $ do
        beam <- pathParam "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
-}

--creates the classes and starts the main game loop
main :: IO ()
main = do
        let encounter = getBasicEncounter1
        let turnList = reverse $ sort encounter
        gameLoop turnList


--top level function responsible for keeping track of current turn 
--and handling input parsing
gameLoop :: [Class] -> IO ()
gameLoop [] = putStrLn "\nNeither team has any characters still capable of fighting, you have both lost today"
gameLoop chars@(turnChar:_) = do
    putStrLn ("Start of " ++ name turnChar ++ "'s turn")
    threadDelay 500000
    if team turnChar == "Friend" then do
        putStrLn "Friends' Stats:"
        printField (friendTeam chars)
        threadDelay 500000
        putStrLn "Enemies' Stats:"
        printField (enemyTeam chars)
        threadDelay 500000
        printActions turnChar
        results <- processInput chars
        checkForEndOfBattle results
    else do
        putStrLn (name turnChar ++ " attacks\n")
        threadDelay 500000
        results <- enemyActions chars
        checkForEndOfBattle results

--displays state of all characters currently
printField :: [Class] -> IO ()
printField [] = putStr ""
printField (curChar : chars) = do
    print curChar
    putStr "\n"
    printField chars

printActions :: Class -> IO()
printActions char = do
    putStrLn "Tutorial:"
    putStrLn "Below are all the possible actions the current turn character can perform"
    putStrLn "The command line entry to perform any given action will be to the left of each action in parentheses"
    putStrLn "<target> should be replaced with the name of a character the action should be"
    putStrLn "performed on, actions which target all, will only target the relevant characters"
    putStrLn "i.e. Stamina Attack All will only stamina attack enemie and not friendlies"
    putStrLn "<1|2|3> indicates an action has multiple levels it can be performed at"
    putStrLn "and what those levels are. Higher level actions will require more Ki/Stamina"
    putStrLn "if a character does not have enough ki/stamina to perform a given action"
    putStrLn "their turn will be spent without performing an action\n"

    threadDelay 500000

    --printing items
    putStrLn (name char ++ "'s Items:")
    displayItem (numHealthPotions itemList) "(HeP <target>) Health Potions: "
    displayItem (numManaPotions itemList) "(MP <target>) Mana Potions: "
    displayItem (numThrowingKnives itemList) "(TK <target>) Throwing Knives: "
    displayItem (numMagicalSeals itemList) "(MS <target>) Magical Seals: "
    displayItem (numWebTraps itemList) "(WT <target>) Web Traps: "
    displayItem (numHastePotions itemList) "(Hap <target>) Haste Potions: "

    threadDelay 500000

    --printing actions
    putStrLn ("\n" ++ name char ++ "'s Actions:")
    displayAction (stamAttack char) "(SA <0|1|2|3> <target>) Stamina Attack Single\n(SAA <1|2>) Stamina Attack All"
    displayAction (kiAttack char) "(KA <0|1|2|3> <target>) Ki Attack Single\n(KAA <1|2>) Ki Attack All"
    displayAction (heal char) "(Hl <1|2|3> <target>) Heal Single\n(HlA <1|2>) Heal All"
    displayAction (rally char) "(Rly <1|2|3> <target>) Rally Single\n(RlyA <1|2>) Rally All"
    displayAction (invigorate char) "(Invig <target>) Invigorate Single\n(InvigA) Invigorate All"
    displayAction (demoralize char) "(Demor <target>) Demoralize Single\n(DemorA) Demoralize All"
    displayAction (intimidate char) "(Intim <target>) Intimidate Single\n(IntimA) Intimidate All"
    displayAction (shield char) "(Shld <target>) Shield Single\n(ShldA) Shield All"
    displayAction (amplify char) "(Amp <target>) Amplify Single\n(AmpA) Amplify All"
    displayAction (dampen char) "(Damp <target>) Dampen Single\n(DampA) Dampen All"
    displayAction (curse char) "(Crs <target>) Curse Single\n(CrsA) Curse All"
    displayAction (barrier char) "(Brr <target>) Barrier Single\n(BrrA) Barrier All"
    where
        itemList = items char
        displayItem i str = if i > 0 then putStrLn (str ++ show i) else putStr ""
        displayAction b str = if b then putStrLn str else putStr ""

--checking if either team has won the battle
--will return f if friendly team has won battle
--will return e if enemy team has won battle
--will return n if neither team has won battle
checkForEndOfBattle :: [Class] -> IO ()
checkForEndOfBattle chars
  | enemyTeamSize == 0 = putStrLn "\nA winner is you!"
  | friendlyTeamSize == 0 = putStrLn "\nA loser is you!"
  | otherwise = gameLoop chars
    where
        enemyTeamSize = length (enemyTeam chars)
        friendlyTeamSize = length (friendTeam chars)

--helper functions for getting characters on a specific team
enemyTeam :: [Class] -> [Class]
enemyTeam chars = filter (\c -> team c == "Enemy") chars

friendTeam :: [Class] -> [Class]
friendTeam chars = filter (\c -> team c == "Friend") chars


--helper function for gameloop which repeatedly calls performAction until a valid input is given
--unwraps the list of all updated characters from perform action and will print out if any characters died
processInput :: [Class] -> IO [Class]
processInput chars@(user:_) = do
    action <- getLine
    let result = performAction (words action) chars
    if isNothing result
        then do
            putStrLn "Unrecognized Command"
            processInput chars
    else do
        let justResult = fromJust result
        printDeadChars $ chars \\ justResult
        if user `notElem` justResult then do
            putStrLn "Turn Character has died, turn order will be reset"
            return justResult
        else return $ fixTurnOrder user justResult

processInput _ = undefined

--helper function for printing out which characters died
printDeadChars :: [Class] -> IO ()
printDeadChars [] = putStr ""
printDeadChars (char:chars) = do
    putStrLn (name char ++ " has perished\n")
    threadDelay 500000
    printDeadChars chars

--after we re-organize everyone by speed we need to re-iterate back through
--whose turn it currently is
fixTurnOrder :: Class -> [Class] -> [Class]
fixTurnOrder turnChar (char:chars) =
    if turnChar == char then chars ++ [char]
    else fixTurnOrder turnChar (chars ++ [char])
fixTurnOrder _ _ = undefined


--takes a string, attempts to parse it, checks that formatting is valid and that the given character can in fact perform the action
--then returns an updated list of all characters
performAction :: [String] -> [Class] -> Maybe [Class]

--use health potion
performAction ("HeP":targetName:_) chars = useItem targetName chars numHealthPotions useHealthPotion

--use mana potion
performAction ("MP":targetName:_) chars = useItem targetName chars numManaPotions useManaPotion

--use throwing knives
performAction ("TK":targetName:_) chars = useItem targetName chars numThrowingKnives useThrowingKnives

--use magial seal
performAction ("MS":targetName:_) chars = useItem targetName chars numMagicalSeals useMagicalSeal

--use web trap
performAction ("WT":targetName:_) chars = useItem targetName chars numWebTraps useWebTraps

--use haste potion
performAction ("HaP":targetName:_) chars = useItem targetName chars numHastePotions useHastePotion

--use stamina attack
performAction ("SA":level:targetName:_) chars = performSingleLevelAction (level:[targetName]) chars [0,1,2,3] stamAttack staminaSingleAttack

--use group stamina attack
--performAction ("SAA":level:_) chars = performGroupLevelAction level chars [1,2] enemyTeam stamAttack staminaGroupAttack

--use ki attack
performAction ("KA":level:targetName:_) chars = performSingleLevelAction (level:[targetName]) chars [0,1,2,3] kiAttack kiSingleAttack

--use group ki attack
--performAction ("KAA":level:_) chars = performGroupLevelAction level chars [1,2] enemyTeam kiAttack kiGroupAttack

--use heal
performAction ("Hl":level:targetName:_) chars = performSingleLevelAction (level:[targetName]) chars [1,2,3] heal healSingle

--use group heal
--performAction ("HlA":level:_) chars = performGroupLevelAction level chars [1,2] friendTeam heal healGroup

--use rally
performAction ("Rly":level:targetName:_) chars = performSingleLevelAction (level:[targetName]) chars [1,2,3] rally rallySingle

--use group rally
--performAction ("RlyA":level:_) chars = performGroupLevelAction level chars [1,2] friendTeam rally rallyGroup

--use invigorate
performAction ("Invig":targetName:_) chars = performStatusSingle targetName chars invigorate invigorateSingle

--use group invigorate
--performAction ("InvigA":_) (user:chars) = undefined

--use demoralize
performAction ("Demor":targetName:_) chars = performStatusSingle targetName chars demoralize demoralizeSingle

--use group demoralize
--performAction ("DemorA":_) (user:chars) = undefined

--use intimidate
performAction ("Intim":targetName:_) chars = performStatusSingle targetName chars intimidate intimidateSingle

--use group intimidate
--performAction ("IntimA":_) (user:chars) = undefined

--use shield
performAction ("Shld":targetName:_) chars = performStatusSingle targetName chars shield shieldSingle

--use group shield 
--performAction ("ShldA":_) (user:chars) = undefined

--use amplify
performAction ("Amp":targetName:_) chars = performStatusSingle targetName chars amplify amplifySingle

--use group amplify 
--performAction ("AmpA":_) (user:chars) = undefined

--use dampen
performAction ("Damp":targetName:_) chars = performStatusSingle targetName chars dampen dampenSingle

--use group dampen
--performAction ("DampA":_) (user:chars) = undefined

--use curse
performAction ("Crs":targetName:_) chars = performStatusSingle targetName chars curse curseSingle

--use group curse
--performAction ("CrsA":_) (user:chars) = undefined

--use barrier
performAction ("Brr":targetName:_) chars = performStatusSingle targetName chars barrier barrierSingle

--use group barrier
--performAction ("BrrA":_) (user:chars) = undefined

performAction _ _ = Nothing

--helper function for finding a target amongst a list of Classes
getTarget :: [Class] -> String -> Maybe Class
getTarget chars targetName =
    if length target == 1 then Just $ head target
    else Nothing
    where
        target = filter (\c -> name c == targetName) chars

--helper function for replacing all the old characters in the turn list
--filtering out all the dead characters
--then re-organizing them into turn order
updateCharList :: [Class] -> [Class] -> [Class]
updateCharList oldList updatedChars = reverse $ sort aliveChars
    where
        unaffectedChars = oldList \\ updatedChars
        aliveChars = filter (\c -> ki (character c) > 0 && stamina (character c) >0) (updatedChars ++ unaffectedChars)

--generic function for using an item
useItem :: String -> [Class] -> (ItemList -> Int) -> (Character -> ItemList -> (Character, ItemList)) -> Maybe [Class]
useItem targetName chars@(user:_) numItem itemOp =
    if isNothing target || numItem (items user) == 0 then Nothing
    else Just returnList
    where
        target = getTarget chars targetName
        justTarget = fromJust target
        outcome = itemOp (character justTarget) (items user)
        returnList =
            if justTarget == user
                then updateCharList chars [updateItems (updateCharacter user (fst outcome)) (snd outcome)]
            else updateCharList chars [updateItems user (snd outcome), updateCharacter justTarget (fst outcome)]

useItem _ _ _ _ = undefined

--generic function for using single target actions which take a level
performSingleLevelAction :: [String] -> [Class] ->  [Int] -> (Class -> Bool) -> (Character -> Character -> Int -> (Character, Character)) -> Maybe [Class]
performSingleLevelAction (level:targetName:_) chars@(user:_) validLevels hasAction action =
    if isNothing target || isNothing levelMaybe || fromJust levelMaybe `notElem` validLevels || not (hasAction user)
        then Nothing
    else Just returnList
    where
        target = getTarget chars targetName
        justTarget = fromJust target
        levelMaybe = readMaybe level :: Maybe Int
        outcome = action (character user) (character justTarget) (fromJust levelMaybe)
        returnList =
            if justTarget == user
                then updateCharList chars [updateCharacter (updateCharacter user (fst outcome)) (snd outcome)]
            else updateCharList chars [updateCharacter user (fst outcome), updateCharacter justTarget (snd outcome)]

performSingleLevelAction _ _ _ _ _ = undefined

--generic function for using group actions which take a level
--need to refactor character to also have name so we can match character to class
{-
performGroupLevelAction :: String -> [Class] -> [Int] -> ([Class] -> [Class]) -> 
    (Class -> Bool) -> (Character -> [Character] -> Int -> (Character, [Character])) -> Maybe [Class]

performGroupLevelAction level chars@(user:_) validLevels filterTeam hasAction action = 
    if isNothing levelMaybe || fromJust levelMaybe `notElem` validLevels || not (hasAction user) then Nothing
    else Just returnList
    where
        levelMaybe = readMaybe level :: Maybe Int
        targetChars = [character curChar | curChar <- filterTeam chars]
        outcome = action (character user) targetChars (fromJust levelMaybe)
        --if this doesnt work how I want it to may have to rethink large parts of program or cut group attacks for the time being
        updatedTargets = [updateCharacter curClass curChar | curClass <- filterTeam chars, curChar <- snd outcome]
        returnList = 
            if user `elem` updatedTargets 
                then undefined
            else updateCharacter user (fst outcome) : updatedTargets

performGroupLevelAction _ _ _ _ _ _ = undefined
-}

--generic function for using a single target status move
performStatusSingle :: String -> [Class] -> (Class -> Bool) -> (Character -> Character -> (Character, Character)) -> Maybe [Class]
performStatusSingle targetName chars@(user:_) hasAction action =
    if isNothing target || not (hasAction user) then Nothing
    else Just returnList
    where
        target = getTarget chars targetName
        justTarget = fromJust target
        outcome = action (character user) (character justTarget)
        returnList =
            if justTarget == user
                then updateCharList chars [updateCharacter (updateCharacter user (fst outcome)) (snd outcome)]
            else updateCharList chars [updateCharacter user (fst outcome), updateCharacter justTarget (snd outcome)]

performStatusSingle _ _ _ _ = undefined

enemyActions :: [Class] -> IO [Class]
enemyActions chars@(turnChar:_) = do
    if stamAttack turnChar then do
        putStrLn "happen1"
        result <- enemyAttack chars stamina staminaSingleAttack
        return $ fixTurnOrder turnChar result
    else do
        putStrLn "happen2"
        result <- enemyAttack chars ki kiSingleAttack
        return $ fixTurnOrder turnChar result

enemyActions _ = undefined

enemyAttack :: [Class] -> (Character -> Int) -> (Character -> Character -> Int -> (Character, Character)) -> IO [Class]
enemyAttack chars@(turnChar:_) stat attack = do
    putStrLn "Chosen target"
    print bestTarget
    putStrLn "unprocessed result of attack"
    print outcome
    return returnList 
    where 
        potentialTargets = friendTeam chars
        bestTarget = minimumBy (\c1 c2 -> compare (stat (character c1)) (stat (character c2))) potentialTargets
        outcome = attack (character turnChar) (character bestTarget) 0
        returnList =
            if turnChar == bestTarget
                then updateCharList chars [updateCharacter (updateCharacter turnChar (fst outcome)) (snd outcome)]
            else updateCharList chars [updateCharacter turnChar (fst outcome), updateCharacter bestTarget (snd outcome)]

enemyAttack _ _ _ = undefined