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

module Hdsk.ModelSelection
( kfold
, gridSearch
, SplitData
, Train, Test
, trainTestSplit
) where

import Hdsk.Preprocessing (PreprocessedBy)

-- | K-Folds cross-validator
kfold = undefined

-- | Evalualte model performance for each configuration of its parameter space.
gridSearch = undefined

-- | Tagging and tracking partitioned data. A term of type @'SplitData' t a@
-- refers to a partition of a dataset of type @a@ tagged with annotation @t@,
-- commonly either 'Train' or 'Test'.
newtype SplitData t a = SplitData a

-- | A tag for 'SplitData' which marks the training partition of a dataset.
data Train
-- | A tag for 'SplitData' which marks the testing partition of a dataset.
data Test

-- | Split a dataset into a training and a testing partition. Note that the
-- preprocessing stack must be empty; this prevents preprocessors from leaking
-- information from the testing partition into model training.
trainTestSplit :: PreprocessedBy '[] (f a) -> (SplitData Train a, SplitData Test a)
trainTestSplit = undefined
