{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Hdsk.DecisionTree
Description : Tools for training and running decision trees
Maintainer  : Badart_William@bah.com
Stability   : experimental
Portability : POSIX

TODO
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Hdsk.DecisionTree
( decisionTreeClassifier
, decisionTreeRegressor
, exportGraphviz
) where

import GHC.TypeLits (ErrorMessage(..))

import Hdsk.Preprocessing
import Hdsk.Internal.Types

-- | Trains a decision tree classifier.
decisionTreeClassifier :: Member algs MissingValueHandler (
       Text "This decision tree algorithm doesn't know what to do with missing values."
  :$$: Text "Please apply a MissingValueHandler first."
  ) => PreprocessedBy algs (f a) -> f label
decisionTreeClassifier = undefined

-- | Trains a decision tree regressor.
decisionTreeRegressor = undefined

-- | Renders a decision tree object to graphviz.
exportGraphviz = undefined
