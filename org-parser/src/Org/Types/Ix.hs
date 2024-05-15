{-# LANGUAGE TemplateHaskell #-}

module Org.Types.Ix where

import Control.Category.Natural (type (~~>))
import Data.Type.Equality (type (:~:) (..))

data family S (a :: ix)

class SingI ix where sing :: S ix

data OrgIx = ObjIx | ElmIx | SecIx

data instance S (a :: OrgIx) where
  SObjIx :: S ObjIx
  SElmIx :: S ElmIx
  SSecIx :: S SecIx

instance SingI ObjIx where sing = SObjIx
instance SingI ElmIx where sing = SElmIx
instance SingI SecIx where sing = SSecIx

class TestIxEq (c :: k -> Type) where
  testIxEq :: S ix -> c jx -> Maybe (ix :~: jx)

-- instance (TestIxEq (c k), Foldable f, Functor f) => TestIxEq (ComposeIx f c k) where
--   testIxEq s (ComposeIx x) = msum (testIxEq s <$> x)

mapIx ::
  forall ix k s.
  (SingI ix, TestIxEq k) =>
  (k ~~> s) ->
  (k ix -> s ix) ->
  k ~~> s
mapIx def f x =
  case testIxEq (sing @ix) x of
    Just Refl -> f x
    Nothing -> def x

getsIx ::
  forall ix k b.
  (SingI ix, TestIxEq k) =>
  (forall jx. k jx -> b) ->
  (k ix -> b) ->
  forall jx.
  k jx ->
  b
getsIx def f x =
  case testIxEq (sing @ix) x of
    Just Refl -> f x
    Nothing -> def x

type AllOrgIx :: (Type -> Constraint) -> (OrgIx -> Type) -> Constraint
type AllOrgIx c k = (c (k ObjIx), c (k ElmIx), c (k SecIx))

type (:+:) = Compose
