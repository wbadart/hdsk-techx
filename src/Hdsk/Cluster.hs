{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Hdsk.Cluster
Description : Data clustering functions
Maintainer  : Badart_William@bah.com
Stability   : experimental
Portability : POSIX

Here we implement a number of different clustering algorithms.
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Hdsk.Cluster where
-- ( dbscan
-- , kmeans
-- ) where

import GHC.TypeLits (TypeError, ErrorMessage(..))
import Hdsk.Preprocessing  -- (Normalizer, PreprocessedBy, otherFunc, standardScaler)

-- | Use DBSCAN to compute a clustering of the pre-normalized dataset.
dbscan :: Normalizer alg => PreprocessedBy alg (f a) -> f clusterId
dbscan = undefined

-- | Calculate the clustering of the pre-normalized dataset using KMeans.
-- TODO: check assumption of convex clusters, as well as choice of /K/?
kmeans :: Normalizer alg => PreprocessedBy alg (f a) -> f clusterId
kmeans = undefined

kmeans' :: Member algs StandardScaler (
       Text "KMeans doesn't give meaningful results on unnormalized data."
  :$$: Text "Please apply a Normalizer to your data first."
  ) => PreprocessedBy' algs (f a) -> f clusterId
kmeans' = undefined

x = [[1, 2, 3], [4, 5, 6]]
x' = noPreprocess x
-- res = kmeans x
-- res' = kmeans' $ x
