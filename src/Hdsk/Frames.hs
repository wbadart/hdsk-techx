{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Hdsk.Frames
Description : Data frame abstraction
Maintainer  : Badart_William@bah.com
Stability   : experimental
Portability : POSIX

TODO
-}

{-# LANGUAGE DataKinds #-}

module Hdsk.Frames
( Record(..)
, readCSV
) where

import Hdsk.Preprocessing (PreprocessedBy, noPreprocess)

-- | A 'Record' type represents an individual data object in the target domain.
-- It's interface is a sort of simplified 'Read', which parses one line of text
-- into a 'Record'.
class Record r where
  -- | Parse a line (or any string) into an @r@.
  parseLine
    :: String  -- ^ Arbitrary string parameter, typically @delim@
    -> String  -- ^ The text to parse
    -> r

-- | Read a list of 'Record's from tabluar data on disk, and record that it is
-- raw data (i.e. no preprocessing has yet been done).
readCSV
  :: Record r
  => String    -- ^ Delimiter
  -> FilePath  -- ^ Path to data
  -> IO (PreprocessedBy '[] [r])
readCSV delim path = noPreprocess <$> readCSV' delim path

readCSV' :: Record r => String -> FilePath -> IO [r]
readCSV' delim path = map (parseLine delim) . lines <$> readFile path
