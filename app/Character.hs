module Character 
( Character(..) ) where

-- data Stamina = Stamina Int deriving (Show)
-- data Ki      = Ki Int deriving (Show)
-- data Speed   = Speed Int deriving (Show, Eq, Ord)
-- data Team    = Friend | Enemy deriving (Show)

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