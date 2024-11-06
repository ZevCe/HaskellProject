module Character 
( Character(..) ,
  Stamina(..),
  Ki(..),
  Speed(..) ) where

data Stamina = Stamina Int deriving (Show)
data Ki      = Ki Int deriving (Show)
data Speed   = Speed Int deriving (Show)

data Character  = Character Stamina Ki Speed deriving (Show)