module Character 
( Character(..) ) where

-- data Stamina = Stamina Int deriving (Show)
-- data Ki      = Ki Int deriving (Show)
-- data Speed   = Speed Int deriving (Show, Eq, Ord)
-- data Team    = Friend | Enemy deriving (Show)

data Character  = 
    Character { stamina :: Int, 
                ki      :: Int,
                speed   :: Int,
                team    :: String }

instance Eq Character where
    c1 == c2 = (speed c1) == (speed c2)

instance Ord Character where
    compare c1 c2 = compare (speed c1) (speed c2)

instance Show Character where
    show (Character sta ki speed team) =
        "Character = " ++ "Stamina: " ++ show sta ++ " Ki: " ++ show ki ++
        " Speed: " ++ show speed ++ " Team: " ++ show team