module Character 
( Character(..) ,
  Stamina(..),
  Ki(..),
  Speed(..),
  Team(..) ) where

data Stamina = Stamina Int deriving (Show)
data Ki      = Ki Int deriving (Show)
data Speed   = Speed Int deriving (Show)
data Team    = Friend | Enemy deriving (Show)

data Character  = Character Stamina Ki Speed Team deriving (Show)