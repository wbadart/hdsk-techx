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
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Hdsk.Cluster
( dbscan
, kmeans
) where

import GHC.TypeLits (ErrorMessage(..))

import Hdsk.Internal.Types (Member)
import Hdsk.Preprocessing (Normalizer, PreprocessedBy)

-- | Use DBSCAN to compute a clustering of the pre-normalized dataset.
dbscan :: Member algs Normalizer (
       'Text "DBSCAN works best with normalized data."
  ':$$: 'Text "Please apply a Normalizer to your data before moving on."
  ) => PreprocessedBy algs (f a) -> f clusterId
dbscan = undefined

kmeans
  :: Member algs Normalizer
          ( Text "Clustering only gives meaningful results for normalized data. "
       :$$: Text "    See https://bit.ly/2FUHTai for further explanation."
       :$$: Text "Please apply a normalizer to your data before clustering. "
       :$$: Text "    See https://TODO for a list of normalizers." )
  => Int -> PreprocessedBy algs df -> [cluster]
kmeans = undefined
