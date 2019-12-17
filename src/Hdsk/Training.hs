{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Hdsk.Training
Description : Training, testing, and validation utilities
Maintainer  : Badart_William@bah.com
Stability   : experimental
Portability : POSIX

TODO
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Hdsk.Training where

import GHC.TypeLits (TypeError, ErrorMessage(..))

import Hdsk.Preprocessing

newtype SplitData t a = SplitData a
data Train
data Test

trainTestSplit :: PreprocessedBy' '[] (f a) -> (SplitData Train a, SplitData Test a)
trainTestSplit = undefined
