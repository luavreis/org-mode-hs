-- | Some convenience functions built around the recusion-schemes API.
module Org.Types.Walk where

import Control.Category.Natural (NProd (..), type (~>) (..), type (~~>))
import Data.Ix.Foldable (IFoldable (..))
import Data.Ix.Functor (IFunctor, ifmap)
import Data.Ix.RecursionSchemes qualified as R
import Org.Types.Variants.Basic
import Data.Ix.Traversable (ITraversable, itraverse)

walk :: (IFunctor f) => (OrgF (Org f) ~~> OrgF (Org f)) -> Org f ~~> Org f
walk f = (R.hoist g #)
  where
    g = NT \(OrgNodes x) -> OrgNodes (ifmap f x)

walkF :: forall f t. (ITraversable f, Applicative t) => (forall i. OrgF (Compose t (Org f)) i -> t (OrgF (Org f) i)) -> forall i. Org f i -> t (Org f i)
walkF f = getCompose . (R.transverse g #)
  where
    g :: OrgNodes f (Compose t (Org f)) ~> Compose t (OrgNodes f (Org f))
    g = NT \(OrgNodes x) -> Compose $ OrgNodes <$> itraverse f x

query :: (IFoldable f, IFunctor f, Monoid m) => (forall jx. OrgF (Const m) jx -> m) -> Org f ix -> m
query f = getConst . (R.fold g #)
  where
    g = NT $ \(OrgNodes nodes) -> Const $ ifoldMap f nodes

queryTopDown :: (IFoldable f, IFunctor f, Monoid m) => (forall jx. OrgF (Org f) jx -> m) -> Org f ix -> m
queryTopDown f = getConst . (R.para' g #)
  where
    g = NT $ \(Org original :.: child) -> Const $ ifoldMap f original <> ifold child

queryBottomUp :: (IFoldable f, IFunctor f, Monoid m) => (forall jx. OrgF (Org f) jx -> m) -> Org f ix -> m
queryBottomUp f = getDual . queryTopDown (Dual . f)

-- * Query

-- queryTopDown :: forall k m a. (R.Recursive (~>) k, Monoid m, IFoldable (R.Base k), GeneralizeQuery k a) => (forall jx. k jx -> m) -> a -> m
-- queryTopDown f = generalizeQuery $ getConst . (R.para' (NT $ \(original :.: child) -> Const $ f original <> ifold child) #)

-- queryBottomUp :: forall k m a. (R.Recursive (~>) k, Monoid m, IFoldable (R.Base k), GeneralizeQuery k a) => (forall jx. k jx -> m) -> a -> m
-- queryBottomUp f = generalizeQuery $ getConst . (R.para' (NT $ \(original :.: child) -> Const $ ifold child <> f original) #)

-- class GeneralizeQuery k a where
--   generalizeQuery :: (Monoid m) => (forall jx. k jx -> m) -> a -> m

-- instance GeneralizeQuery k (k ix) where
--   generalizeQuery f = f

-- instance (IFoldable f) => GeneralizeQuery (OrgF (Org f)) (Org f ix) where
--   generalizeQuery f (Org x) = ifoldMap f x
