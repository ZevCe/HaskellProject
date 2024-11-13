## Modules
<p>As of right now we have the modules: ModifyStats, Test, Actions, Main, Item, and Character.
ModifyStats, Actions, Item, and Character are calculations and data types used in Main, which will manage the game and handle frontend commmunications.
There may be other modules for more types of calculations, but right now we have the necessities and basics.
The Test module is used to test the above modules. Currently it uses a scenario that simply test all of the above modules as well as a unit test using HUnit for individual modules.
The Character module will use Scotty to manage the frontend, but this hasn't been implemented very much yet.
We still need to make a JSON management module in order to send data from and to the frontend and backend. We haven't started on this as we are still working on the basic modules, data types, and functions, but we plan on the Aeson library to help us out when we do get around to it.</p>

## Data Structures
<p>The only data structure we have right now is the Character data type. It holds a multitude of int, string, and string array variables that hold everything we need to hold a character object. This character data type holds all the data needed for all the calculations we have in our modules.</p>

## Functions
<p>Modify Status holds functions makeCharacter, modifyStamina, modifyKi, modifySoeed, and modifyStatuses. makeCharacter is makes a new character with inputs Int -> Int -> Int -> Int -> Int -> [String] -> String. The modify functions aren't complete, but eventually they will take a character and an int/string and return a character.<br>
Actions holds staminaSingleAttack, kiSingleAttack, staminaGroupAttack, kiGroupAttack, and healingSingle. staminaSingleAttack takes Character -> Character -> Int and returns a (Character, Character) which holds the resulting attacker and defender characters post attack. staminaGroupAttack is the same, but the defending character is instead a character list for both input and output. kiSingleAttack takes Character -> Character -> Int -> (Character, Character) just like stamina attacks and kiGroupAttack also has defender [Character] like staminaGroupAttack. healingSingle follows the same inputs and outputs as kiSingleAttack.<br>
Item holds useHealthPotion, which takes Character and it returns Character that has an increased Stamina.<br>
Test holds getTurnOrder, which takes a list of characters and returns the list in order of speed, and testItems, which uses HUnit to test the useHealthPotion function. </p>

## Testing
<p>We will be doing scenario testing, where we simulate a combat encounter with a couple of turns, and unit testing using HUnit. Our scenario tests 5 characters using Action.hs functions and Item.hs functions. It prints the result of each action so we can compare it with the our own prediction of what should happen. We also have a simple unit test for Item.hs. We still need to write unit tests for more functions, but testItems is our currently implementation of how our unit tests will look like.<\p>

## Questions
<p>We have no quesitons at this point.</p>