module Character 
( Character(..) ) where

data Character  = 
    Character { stamina    :: Int, 
                maxStamina :: Int,
                ki         :: Int,
                maxKi      :: Int,
                speed      :: Int,
                statuses   :: [String],
                team       :: String }


instance Eq Character where
    c1 == c2 = (speed c1) == (speed c2)

instance Ord Character where
    compare c1 c2 = compare (speed c1) (speed c2)

instance Show Character where
    show c =
        "Character = " ++ "Stamina: " ++ show (stamina c) ++ "/" ++ show (maxStamina c) ++ 
        " Ki: " ++ show (ki c) ++ "/" ++ show (maxKi c) ++ 
        " Speed: " ++ show (speed c) ++ " Statuses: " ++ show (statuses c) ++ 
        " Team: " ++ show (team c)

makeCharacter :: Int -> Int -> Int -> String -> Character
makeCharacter sta k spd t = 
    Character sta sta k k spd [] t

modifyStamina :: Character -> Int -> Character
modifyStamina char mod = 
    Character (stamina char + mod) (maxStamina char) (ki char) (maxKi char) (speed char) (statuses char) (team char)

modifyKi :: Character -> Int -> Character
modifyKi char mod =  
    Character (stamina char) (maxStamina char) (ki char + mod) (maxKi char) (speed char) (statuses char) (team char)

modifySpeed :: Character -> Int -> Character
modifySpeed char mod = 
    Character (stamina char) (maxStamina char) (ki char) (maxKi char) (speed char + mod) (statuses char) (team char)

modifyStatuses :: Character -> [String] -> Character
modifyStatuses char newStatuses = 
    Character (stamina char) (maxStamina char) (ki char) (maxKi char) (speed char) (newStatuses) (team char)