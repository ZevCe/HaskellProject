## Building Project

Type `cabal install` into terminal to build/run project when in the folder with
HaskellProject.cabal.

## Running Server

To run the server, type `cabal run` into terminal when in the folder with 
HaskellProject.cabal.

Visit the site via http://localhost:3000


## Main Components
### Main.hs/Scotty
Using the scotty library sets up our server and creates the routes for internal api requests.
### Character.hs
Defines all our datatypes and provides operations for modifying them.
### Action.hs
Where all the processing of the api calls happens, updates the provided characters based on the specified action and then returns the updated list of characters.
### Encounter.hs
Functions for building and loading new encounters to be passed to the front end on startup.
### Aeson
Used for passing our data between the backend and front end in the form of JSONs.
### Test.hs/HUnit
All of our unit tests.
### Frontend
All of the js/css/html needed to make the front end happen, no haskell code but does work closely with the haskell code and a substatial portion of the project.