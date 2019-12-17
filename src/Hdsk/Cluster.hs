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

module Hdsk.Cluster
( dbscan
, kmeans
) where

import GHC.TypeLits (ErrorMessage(..))
import Hdsk.Preprocessing (Normalizer, PreprocessedBy)
import Hdsk.Internal.Types (Member)

-- | Use DBSCAN to compute a clustering of the pre-normalized dataset.
dbscan :: Member algs Normalizer (
       Text "DBSCAN works best with normalized data."
  :$$: Text "Please apply a Normalizer to your data before moving on."
  ) => PreprocessedBy algs (f a) -> f clusterId
dbscan = undefined

-- | Calculate the clustering of the pre-normalized dataset using KMeans.
-- TODO: check assumption of convex clusters, as well as choice of /K/?
kmeans :: Member algs Normalizer (
       Text "KMeans doesn't give meaningful results on unnormalized data."
  :$$: Text "Please apply a Normalizer to your data first."
  ) => PreprocessedBy algs (f a) -> f clusterId
kmeans = undefined
