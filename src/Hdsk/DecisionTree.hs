{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Hdsk.DecisionTree
Description : Tools for training and running decision trees
Maintainer  : Badart_William@bah.com
Stability   : experimental
Portability : POSIX

TODO
-}

module Hdsk.DecisionTree
( decisionTreeClassifier
, decisionTreeRegressor
, exportGraphviz
) where

import Hdsk.Preprocessing

-- | Trains a decision tree classifier.
decisionTreeClassifier :: MissingValueHandler alg => PreprocessedBy alg (f a) -> f label
decisionTreeClassifier = undefined

x = keepMissingVals $ standardScaler [[1, 2, 3], [4, 5, 6]]
res = decisionTreeClassifier x

-- | Trains a decision tree regressor.
decisionTreeRegressor = undefined

-- | Renders a decision tree object to graphviz.
exportGraphviz = undefined
