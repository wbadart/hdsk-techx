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
import Data.Nat
import GHC.TypeLits (TypeError, ErrorMessage(..))

-- | The 'PreprocessedBy' type tags datasets of type @a@ as having been
-- preprocessed by algorithm @alg@. This is useful for requiring certain
-- preprocessing steps to have occurred before modeling.
newtype PreprocessedBy alg a = Preprocessed a
newtype PreprocessedBy' (algs :: [Type]) a = Preprocessed' a
  deriving Show

-- | The 'Normalizer' class tags algorithms which normalize the dataset on
-- which they operate.
class Normalizer alg
class MissingValueHandler alg

type family Member (ts :: [Type]) (t :: Type) (e :: ErrorMessage) :: Constraint where
  Member ts t e =
    ( Find ts t
    , t ~ IndexOf ts (Found ts t e)
    )

type family Found (ts :: [k]) (t :: k) (e :: ErrorMessage) :: Nat where
  Found '[]      t e = TypeError e
  Found (t : ts) t _ = Z
  Found (u : ts) t e = S (Found ts t e)

class Find (ts :: [k]) (t :: k) where
  finder :: SNat (Found ts t (Text "unreachable"))

instance {-# OVERLAPPING #-} Find (t : ts) t where
  finder = SZ
  {-# INLINE finder #-}

instance ( Find ts t
         , Found (u : ts) t (Text "unreachable") ~ S (Found ts t (Text "unreachable"))
         ) => Find (u : ts) t where
  finder = SS $ finder @_ @ts @t
  {-# INLINE finder #-}

type family IndexOf (ts :: [k]) (n :: Nat) :: k where
  IndexOf (k ': ks) 'Z = k
  IndexOf (k ': ks) ('S n) = IndexOf ks n

data DropMissingVals
instance MissingValueHandler DropMissingVals
dropMissingVals :: f a -> PreprocessedBy DropMissingVals (f a)
dropMissingVals = undefined

data KeepMissingVals
instance MissingValueHandler KeepMissingVals
keepMissingVals :: f a -> PreprocessedBy KeepMissingVals (f a)
keepMissingVals = undefined

keepMissingVals' :: PreprocessedBy' algs (f a) -> PreprocessedBy' (KeepMissingVals : algs) (f a)
keepMissingVals' = undefined

data StandardScaler
instance Normalizer StandardScaler
-- | Standardize features by removing the mean and scaling to unit variance.
standardScaler :: f a -> PreprocessedBy StandardScaler (f a)
standardScaler = undefined

standardScaler' :: PreprocessedBy' algs (f a) -> PreprocessedBy' (StandardScaler : algs) (f a)
standardScaler' = undefined

noPreprocess :: f a -> PreprocessedBy' '[] (f a)
noPreprocess = Preprocessed'

data MinMaxScaler
instance Normalizer MinMaxScaler
-- | Transform features by scaling each feature to a given range.
minMaxScaler :: f a -> PreprocessedBy MinMaxScaler (f a)
minMaxScaler = undefined

data OtherFunc
otherFunc :: f a -> PreprocessedBy OtherFunc (f a)
otherFunc = undefined

instance {-# OVERLAPPABLE #-} TypeError (
  Text "This function doesn't give meaningful results on unnormalized data."
  :$$: Text "Please apply a Normalizer to your data first."
  :$$: Text "(TODO: link to listing of Normalizer instances)")
  => Normalizer a

instance {-# OVERLAPPABLE #-} TypeError (
  Text "This function requires you to consider missing values."
  :$$: Text "Please apply a MissingValueHandler to your dataset."
  :$$: Text "(TODO: link to listing of MissingValueHandler instances)")
  => MissingValueHandler a
