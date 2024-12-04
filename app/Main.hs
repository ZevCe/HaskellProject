{-# LANGUAGE OverloadedStrings #-}

--import Data.Aeson
import Web.Scotty
import Character
import Actions (performAction)
import Encounter

main :: IO ()
main = scotty 3000 $ do
    get "/" $ do
      file "./frontend/index.html"

    get "/script.js" $ do
      file "./frontend/script.js"

    get "/style.css" $ do
      file "./frontend/style.css"

    get "/images/temp.png" $ do
      file "./frontend/images/temp.png"

    --get request for loading an encounter  
    get "/loadEncounter/:num" $ do
      numString <- pathParam "num"
      liftIO $ print numString
      json (getEncounter numString)

    --post request for calculating new info for a round
    post "/roundInfo" $ do
        info <- jsonData :: ActionM NetworkPacket
        liftIO $ print info
        json $ performAction (action info) (turnOrder info)