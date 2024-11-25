{-# LANGUAGE OverloadedStrings #-}

--import Data.Aeson
import Web.Scotty
import Character 
import Actions (performAction)

main :: IO ()
main = scotty 3000 $ do
    get "/" $ do
      file "./frontend/index.html"

    get "/script.js" $ do
      file "./frontend/script.js"

    get "/style.css" $ do
      file "./frontend/style.css"

    --get request for loading an encounter  
    get "/loadEncounter/:num" $ do
      html "stop rushing me"

    --post request for calculating new info for a round
    post "/roundInfo" $ do
        info <- jsonData :: ActionM NetworkPacket
        liftIO $ print info
        let newInfo = performAction (action info) (turnOrder info)
        json (NetworkPacket [] newInfo)

-- printActions :: Class -> IO()
-- printActions char = do
--     --printing actions
--     putStrLn ("\n" ++ name char ++ "'s Actions:")
--     displayAction (stamAttack char) "(SA <0|1|2|3> <target>) Stamina Attack Single\n(SAA <1|2>) Stamina Attack All"
--     displayAction (kiAttack char) "(KA <0|1|2|3> <target>) Ki Attack Single\n(KAA <1|2>) Ki Attack All"
--     displayAction (heal char) "(Hl <1|2|3> <target>) Heal Single\n(HlA <1|2>) Heal All"
--     displayAction (rally char) "(Rly <1|2|3> <target>) Rally Single\n(RlyA <1|2>) Rally All"
--     displayAction (invigorate char) "(Invig <target>) Invigorate Single\n(InvigA) Invigorate All"
--     displayAction (demoralize char) "(Demor <target>) Demoralize Single\n(DemorA) Demoralize All"
--     displayAction (intimidate char) "(Intim <target>) Intimidate Single\n(IntimA) Intimidate All"
--     displayAction (shield char) "(Shld <target>) Shield Single\n(ShldA) Shield All"
--     displayAction (amplify char) "(Amp <target>) Amplify Single\n(AmpA) Amplify All"
--     displayAction (dampen char) "(Damp <target>) Dampen Single\n(DampA) Dampen All"
--     displayAction (curse char) "(Crs <target>) Curse Single\n(CrsA) Curse All"
--     displayAction (barrier char) "(Brr <target>) Barrier Single\n(BrrA) Barrier All"
--     where
--         itemList = items char
--         displayItem i str = if i > 0 then putStrLn (str ++ show i) else putStr ""
--         displayAction b str = if b then putStrLn str else putStr ""

-- --checking if either team has won the battle
-- checkForEndOfBattle :: [Class] -> IO ()
-- checkForEndOfBattle chars
--   | enemyTeamSize == 0 = putStrLn "\nA winner is you!"
--   | friendlyTeamSize == 0 = putStrLn "\nA loser is you!"
--   | otherwise = gameLoop chars
--     where
--         enemyTeamSize = length (enemyTeam chars)
--         friendlyTeamSize = length (friendTeam chars)




