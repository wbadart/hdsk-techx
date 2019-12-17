{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Hdsk.Preprocessing
Description : Preprocessing utilities for cleaning, normalizing, etc.
Maintainer  : Badart_William@bah.com
Stability   : experimental
Portability : POSIX

"Hdsk.Preprocessing" exposes a suite of data preprocessing utilities.
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Hdsk.Preprocessing where
-- ( Normalizer
-- , PreprocessedBy
-- , PreprocessedBy'
-- , standardScaler
-- , minMaxScaler
-- , otherFunc
-- ) where

import Data.Kind
import GHC.TypeLits (TypeError, ErrorMessage(..))

-- | The 'PreprocessedBy' type tags datasets of type @a@ as having been
-- preprocessed by algorithm @alg@. This is useful for requiring certain
-- preprocessing steps to have occurred before modeling.
newtype PreprocessedBy (algs :: [Type]) a = Preprocessed a
  deriving (Show, Functor)

-- | Algorithms which are 'Normalizer's scale data in a way that can be said to
-- eliminate units, enabling the comparison of features which, raw, have
-- different scales or even orders of magnitude.
data Normalizer

-- | 'MissingValueHandler's denote that some consideration has been given to
-- missing values within a dataset. This could be anything from dropping all
-- rows with a missing value (as with TODO), to allowing all missing values to
-- pass through (i.e. TODO).
data MissingValueHandler

dropMissingVals :: PreprocessedBy algs a -> PreprocessedBy (MissingValueHandler : algs) (f a)
dropMissingVals = undefined

keepMissingVals :: PreprocessedBy algs a -> PreprocessedBy (MissingValueHandler : algs) (f a)
keepMissingVals = undefined

-- | Standardize features by removing the mean and scaling to unit variance.
standardScaler :: PreprocessedBy algs a -> PreprocessedBy (Normalizer : algs) (f a)
standardScaler = undefined

-- | Transform features by scaling each feature to a given range.
minMaxScaler :: PreprocessedBy algs a -> PreprocessedBy (Normalizer : algs) (f a)
minMaxScaler = undefined

noPreprocess :: f a -> PreprocessedBy '[] (f a)
noPreprocess = Preprocessed
