{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Hdsk.ModelSelection
Description : Utilities for training and evaluating models on selected data
Maintainer  : Badart_William@bah.com
Stability   : experimental
Portability : POSIX

TODO
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}

module Hdsk.ModelSelection where
-- ( kfold
-- , gridSearch
-- , SplitData
-- , Train, Test
-- , trainTestSplit
-- ) where

import Data.The
import Data.Vector.Generic (Vector)
import GHC.TypeLits (ErrorMessage(..))
import Hdsk.Preprocessing (PreprocessedBy)
import Hdsk.Internal.Types (Empty)

import qualified Data.Vector.Generic as V

-- | K-Folds cross-validator
kfold = undefined

-- | Evalualte model performance for each configuration of its parameter space.
gridSearch = undefined

newtype Proportion a = UnsafeProportion { unProportion :: a }
pattern Proportion :: a -> Proportion a
pattern Proportion a <- UnsafeProportion a
instance Show a => Show (Proportion a) where
  show (UnsafeProportion n) = "Proportion " <> show n

proportion :: (Num a, Ord a) => a -> Maybe (Proportion a)
proportion n | n >= 0 && n <= 1 = Just (UnsafeProportion n)
             | otherwise        = Nothing

-- | Tagging and tracking partitioned data. A term of type @'SplitData' t a@
-- refers to a partition of a dataset of type @a@ tagged with annotation @t@,
-- commonly either 'Train' or 'Test'.
newtype SplitData t a = SplitData a
  deriving Show

instance The (SplitData t a) a

-- | A tag for 'SplitData' which marks the training partition of a dataset.
data Train
-- | A tag for 'SplitData' which marks the testing partition of a dataset.
data Test

-- | Split a dataset into a training and a testing partition. Note that the
-- preprocessing stack must be empty; this prevents preprocessors from leaking
-- information from the testing partition into model training.
trainTestSplit ::
  ( Vector v a
  , Ord trainProportion, RealFrac trainProportion
  , Empty algs ('Text "You've done some preprocessing already ("
          ':<>: 'ShowType algs ':<>: 'Text ")."
          ':$$: 'Text "You should split up your train and test partitions "
          ':$$: 'Text "  first to prevent preprocessing algorithms from "
          ':$$: 'Text "  leaking information about test data into training."))
  => Proportion trainProportion -- ^ Proportion of total to put in train partition
  -> PreprocessedBy algs (v a)  -- ^ Un-preprocessed foldable of data
  -> (SplitData Train (v a), SplitData Test (v a)) -- ^ Tagged partitions
trainTestSplit (Proportion ratio) (The df) =
  let len = fromIntegral (V.length df)
      (train, test) = V.splitAt (floor (ratio * len)) df
   in (SplitData train, SplitData test)
