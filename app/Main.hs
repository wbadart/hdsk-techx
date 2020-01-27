{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Text (Text, pack, unpack, splitOn)
import Data.Vector (Vector, fromList)

import Hdsk.Frames

data Career = Career
  { unkR       :: Text
  , educationR :: Double
  , incomeR    :: Int
  , womenR     :: Double
  , prestigeR  :: Double
  , censusR    :: Int
  , type_R     :: Text
  }
  deriving Show

instance Record Career where
  parseLine delim line =
    let cells = splitOn (pack delim) (pack line)
    in Career
      (cells !! 0)
      (read $ (unpack $ cells !! 1))
      (read $ (unpack $ cells !! 2))
      (read $ (unpack $ cells !! 3))
      (read $ (unpack $ cells !! 4))
      (read $ (unpack $ cells !! 5))
      (cells !! 6)

main = do
  df <- readCSV @Career "," "./app/prestige.csv"
  print df
