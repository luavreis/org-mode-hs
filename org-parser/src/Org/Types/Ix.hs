{-# LANGUAGE TemplateHaskell #-}

module Org.Types.Ix where

import Control.Category.Endofunctor (Endofunctor)
import Control.Category.Natural (type (~>))
import Data.Ix.Foldable (IFoldable)
import Data.Ix.Instances
import Generics.Kind.TH (deriveGenericK)

data OrgIx = ObjIx | ElmIx | SecIx

type AllOrgIx :: (Type -> Constraint) -> (OrgIx -> Type) -> Constraint
type AllOrgIx c k = (c (k ObjIx), c (k ElmIx), c (k SecIx))

newtype ComposeIx (f :: Type -> Type) k s ix = ComposeIx (f (k s ix))
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
