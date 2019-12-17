{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

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

data PrestigeFrame = PrestigeFrame
  { unk       :: Vector Text
  , education :: Vector Double
  , income    :: Vector Int
  , women     :: Vector Double
  , prestige  :: Vector Double
  , census    :: Vector Int
  , type_     :: Vector Text
  }
  deriving Show

instance Record Career where
  readLine delim line =
    let cells = splitOn (pack delim) (pack line)
    in Career
      (cells !! 0)
      (read $ (unpack $ cells !! 1))
      (read $ (unpack $ cells !! 2))
      (read $ (unpack $ cells !! 3))
      (read $ (unpack $ cells !! 4))
      (read $ (unpack $ cells !! 5))
      (cells !! 6)

instance Frame PrestigeFrame Career where
  parseCSV delim = map (readLine delim) . lines
  rowsToFrame records = PrestigeFrame
    (fromList $ map unkR records)
    (fromList $ map educationR records)
    (fromList $ map incomeR records)
    (fromList $ map womenR records)
    (fromList $ map prestigeR records)
    (fromList $ map censusR records)
    (fromList $ map type_R records)

main = do
  df <- readCSV @Career @PrestigeFrame "," "/Users/williambadart/Downloads/Prestige2.csv"
  print df
