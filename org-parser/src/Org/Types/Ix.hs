{-# LANGUAGE TemplateHaskell #-}

module Org.Types.Ix where

import Control.Category.Endofunctor (Endofunctor)
import Control.Category.Natural (NProd (..), type (~>) (..), type (~~>))
import Control.Category.RecursionSchemes qualified as R
import Data.Ix.Foldable (IFoldable (ifold), ifoldMap)
import Data.Ix.Instances
import Data.Type.Equality (type (:~:) (..))
import Generics.Kind.TH (deriveGenericK)

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

mapIx ::
  forall ix k s.
  (TestIxEq k) =>
  (k ~~> s) ->
  S ix ->
  (k ix -> s ix) ->
  k ~~> s
mapIx def proxy f (x :: k jx) =
  case testIxEq proxy x of
    Just Refl -> f x
    Nothing -> def x

getsIx ::
  forall ix k b.
  (TestIxEq k) =>
  (forall jx. k jx -> b) ->
  S ix ->
  (k ix -> b) ->
  forall jx. k jx -> b
getsIx def proxy f (x :: k jx) =
  case testIxEq proxy x of
    Just Refl -> f x
    Nothing -> def x

type AllOrgIx :: (Type -> Constraint) -> (OrgIx -> Type) -> Constraint
type AllOrgIx c k = (c (k ObjIx), c (k ElmIx), c (k SecIx))

newtype ComposeIx (f :: Type -> Type) k s ix = ComposeIx {getComposeIx :: f (k s ix)}
  deriving (Eq, Ord, Show, Generic, Typeable)
  deriving anyclass (NFData)

deriving newtype instance (Semigroup (f (k s ix))) => Semigroup (ComposeIx f k s ix)
deriving newtype instance (Monoid (f (k s ix))) => Monoid (ComposeIx f k s ix)

$(deriveGenericK ''ComposeIx)
deriving via
  (Generically (ComposeIx f k))
  instance
    ( Functor f
    , Endofunctor (~>) k
    ) =>
    (Endofunctor (~>) (ComposeIx f k))
deriving via
  (Generically (ComposeIx f k))
  instance
    ( Foldable f
    , IFoldable k
    ) =>
    (IFoldable (ComposeIx f k))

queryTopDown :: forall k m a. (R.Recursive (~>) k, Monoid m, IFoldable (R.Base k), GeneralizeQuery k a) => (forall jx. k jx -> m) -> a -> m
queryTopDown f = generalizeQuery $ getConst . (R.para' (NT $ \(original :.: child) -> Const $ f original <> ifold child) #)

queryBottomUp :: forall k m a. (R.Recursive (~>) k, Monoid m, IFoldable (R.Base k), GeneralizeQuery k a) => (forall jx. k jx -> m) -> a -> m
queryBottomUp f = generalizeQuery $ getConst . (R.para' (NT $ \(original :.: child) -> Const $ ifold child <> f original) #)

class GeneralizeQuery k a where
  generalizeQuery :: Monoid m => (forall jx. k jx -> m) -> a -> m

instance GeneralizeQuery k (k ix) where
  generalizeQuery f = f

instance (IFoldable f) => GeneralizeQuery k (f k ix) where
  {- HLINT ignore "Eta reduce" -}
  generalizeQuery f = ifoldMap f
