{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Hdsk.Frames
Description : Data frame abstraction
Maintainer  : Badart_William@bah.com
Stability   : experimental
Portability : POSIX

TODO
-}

{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies      #-}

module Hdsk.Frames
-- (
-- ) where
where

import Data.Kind
import Language.Haskell.TH hiding (Type)

class Record r where
  readLine :: String -> String -> r

class Record record => Frame frame record where
  rowsToFrame :: [record] -> frame
  parseCSV :: String -> String -> [record]

readCSV :: forall r f. (Record r, Frame f r) => String -> FilePath -> IO f
readCSV delim path = rowsToFrame @f @r . parseCSV @f delim <$> readFile path
