{-# LANGUAGE OverloadedStrings #-}
module Main where
import Character
import Item
import Data.List
import Control.Concurrent
import Data.Maybe


{-
import Web.Scotty

main :: IO ()
main = scotty 3000 $
    get "/:word" $ do
        beam <- pathParam "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
-}

data Class =
    Class {
        character :: Character,
        items     :: ItemList,
        name      :: String,
        team      :: String,
        stamAttack   :: Bool,
        kiAttack     :: Bool,
        heal         :: Bool,
        rally        :: Bool,
        invigorate   :: Bool,
        demoralize   :: Bool,
        intimidate   :: Bool,
        shield       :: Bool,
        amplify      :: Bool,
        dampen       :: Bool,
        curse        :: Bool,
        barrier      :: Bool}

instance Eq Class where
    c1 == c2 = character c1 == character c2

instance Ord Class where
    compare c1 c2 = compare (character c1) (character c2)

instance Show Class where
    show c = name c ++ "\n" ++ show (character c)


--creates the classes and starts the main game loop
main :: IO ()
main = do
    let class1 = Class
                (makeCharacter 100 100 100)
                (ItemList 1 1 1 1 1 1)
                "God" "Friend"
                True True True True True True
                True True True True True True
        class2 = Class
                (makeCharacter 10 10 10)
                (ItemList 0 0 0 0 0 0)
                "Rat" "Enemy"
                True False False False False False
                False False False False False False
        turnList = reverse $ sort [class1, class2]
    gameLoop turnList


--top level function responsible for keeping track of current turn 
--and handling input parsing
gameLoop :: [Class] -> IO ()
gameLoop [] = putStrLn "critical error"
gameLoop (turnChar:chars) = do
    putStrLn ("Start of " ++ name turnChar ++ "'s turn")
    threadDelay 500000
    putStrLn "Friends' Stats:"
    printField (friendTeam (turnChar:chars))
    putStrLn "Enemies' Stats:"
    printField (enemyTeam (turnChar:chars))
    threadDelay 500000
    if team turnChar == "Friend"
        then do
            printActions turnChar
            results <- parseInput (turnChar:chars)
            gameLoop (chars ++ [turnChar])
    else do
        putStrLn (name turnChar ++ " attacks")
        gameLoop (chars ++ [turnChar])

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

    --printing items
    putStrLn "Items:"
    displayItem (numHealthPotions itemList) "(HeP <target>) Health Potions: "
    displayItem (numManaPotions itemList) "(MP <target>) Mana Potions: "
    displayItem (numThrowingKnives itemList) "(TK <target>) Throwing Knives: "
    displayItem (numMagicalSeals itemList) "(MS <target>) Magical Seals: "
    displayItem (numWebTraps itemList) "(WT <target>) Web Traps: "
    displayItem (numHastePotions itemList) "(Hap <target>) Haste Potions: "

    --printing actions
    putStrLn "\nActions:"
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
        displayItem :: Int -> String -> IO()
        displayItem i str = if i > 0 then putStrLn (str ++ show i) else putStr ""
        displayAction b str = if b then putStrLn str else putStr ""

--checking if either team has won the battle
--will return f if friendly team has won battle
--will return e if enemy team has won battle
--will return n if neither team has won battle
checkForEndOfBattle :: [Class] -> IO String
checkForEndOfBattle chars
  | enemyTeamSize == 0 = return "f"
  | friendlyTeamSize == 0 = return "e"
  | otherwise = return "n"
    where
        enemyTeamSize = length (enemyTeam chars)
        friendlyTeamSize = length (friendTeam chars)

--helper functions for getting characters on a specific team
enemyTeam :: [Class] -> [Class]
enemyTeam chars = filter (\c -> team c == "Enemy") chars

friendTeam :: [Class] -> [Class]
friendTeam chars = filter (\c -> team c == "Friend") chars


--helper function which repeatedly calls performAction until a valid input is given
parseInput :: [Class] -> IO (Class, [Class])
parseInput (user:chars) = do
    action <- getLine
    let result = performAction (words action) (user:chars)
    maybe ( do
            putStrLn "Unrecognized Command"
            parseInput (user:chars)
            )
            return result

parseInput _ = undefined


--need to check if char has given item/skill before using, otherwise return nothing
--need to check if skill level range if valid, if not return nothing
--if character does not have enough Ki/Stamina to perform action will still take up
--turn, but need to find way to notify user

performAction :: [String] -> [Class] -> Maybe (Class, [Class])

--use health potion
performAction ("HeP":target) (user:chars) = undefined

--use mana potion
performAction ("MP":target) (user:chars) = undefined

--use throwing knives
performAction ("TK":target) (user:chars) = undefined

--use magial seal
performAction ("MS":target) (user:chars) = undefined

--use web trap
performAction ("WT":target) (user:chars) = undefined

--use haste potion
performAction ("HaP":target) (user:chars) = undefined

--use stamina attack
performAction ("SA":level:target) (user:chars) = undefined

--use group stamina attack
performAction ("SAA":level:target) (user:chars) = undefined

--use ki attack
performAction ("KA":level:target) (user:chars) = undefined

--use group ki attack
performAction ("KAA":level:target) (user:chars) = undefined

--use heal
performAction ("Hl":level:target) (user:chars) = undefined

--use group heal
performAction ("HlA":level) (user:chars) = undefined

--use rally
performAction ("Rly":level:target) (user:chars) = undefined

--use group rally
performAction ("RlyA":level) (user:chars) = undefined

--use invigorate
performAction ("Invig":target) (user:chars) = undefined

--use group invigorate
performAction ("InvigA":_) (user:chars) = undefined

--use demoralize
performAction ("Demor":target) (user:chars) = undefined

--use group demoralize
performAction ("DemorA":_) (user:chars) = undefined

--use intimidate
performAction ("Intim":target) (user:chars) = undefined

--use group intimidate
performAction ("IntimA":_) (user:chars) = undefined

--use shield
performAction ("Shld":target) (user:chars) = undefined

--use group shield 
performAction ("ShldA":_) (user:chars) = undefined

--use amplify
performAction ("Amp":target) (user:chars) = undefined

--use group amplify 
performAction ("AmpA":_) (user:chars) = undefined

--use dampen
performAction ("Damp":target) (user:chars) = undefined

--use group dampen
performAction ("DampA":_) (user:chars) = undefined

--use curse
performAction ("Crs":target) (user:chars) = undefined

--use group curse
performAction ("CrsA":_) (user:chars) = undefined

--use barrier
performAction ("Brr":target) (user:chars) = undefined

--use group barrier
performAction ("BrrA":target) (user:chars) = undefined

performAction  _ chars = Nothing