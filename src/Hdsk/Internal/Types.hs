{-|
Module      : Hdsk.Internal.Types
Description : Type-level programming utilities
Maintainer  : Badart_William@bah.com
Stability   : experimental
Portability : POSIX

TODO
-}

{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Hdsk.Internal.Types
( Member
) where

import Data.Kind
import GHC.TypeLits
  ( ErrorMessage(..)
  , TypeError
  , Nat
  , type (+)
  , type (-))

type family Member (ts :: [Type]) (t :: Type) (e :: ErrorMessage) :: Constraint where
  Member ts t e =
    ( Find ts t
    , t ~ IndexOf ts (Found ts t e)
    )

data SNat :: Nat -> Type where
  SZ :: SNat 0
  SS :: SNat n -> SNat (n + 1)

type family IndexOf (ts :: [k]) (n :: Nat) :: Type where
  IndexOf (k : ks) 0 = k
  IndexOf (k : ks) n = IndexOf ks (n - 1)

type family Found (ts :: [k]) (t :: k) (e :: ErrorMessage) :: Nat where
  Found '[]      t e = TypeError e
  Found (t : ts) t _ = 0
  Found (u : ts) t e = (Found ts t e) + 1

class Find (ts :: [k]) (t :: k) where
  finder :: SNat (Found ts t (Text "unreachable"))

instance {-# OVERLAPPING #-} Find (t : ts) t where
  finder = SZ
  {-# INLINE finder #-}

instance ( Find ts t
         , Found (u : ts) t (Text "unreachable") ~ ((Found ts t (Text "unreachable")) + 1)
         ) => Find (u : ts) t where
  finder = SS $ finder @_ @ts @t
  {-# INLINE finder #-}
