{-# LANGUAGE TemplateHaskell #-}
-- |

module Org.Types.Variants.Basic
  ( OrgF (..)
  , secData
  , OrgNodes (..)
  , Org
  , pattern Org

    -- * Re-exports
  , module Org.Types.Data.Document
  , module Org.Types.Data.Section
  , module Org.Types.Data.Element
  , module Org.Types.Data.Object
  , module Org.Types.Data.Timestamp
  , module Org.Types.Ix
  )
where

import Control.Category.Endofunctor (Endofunctor)
import Control.Category.Natural (type (~>))
import Data.Ix.Foldable (IFoldable)
import Data.Ix.Instances
import Data.Ix.RecursionSchemes (Fix (..))
import Data.Ix.Traversable (ITraversable)
import Data.Type.Equality ((:~:) (..))
import Generics.Kind.TH (deriveGenericK)
import Org.Types.Data.Document
import Org.Types.Data.Element
import Org.Types.Data.Object
import Org.Types.Data.Section
import Org.Types.Data.Timestamp
import Org.Types.Ix
import Relude.Extra.Lens (Lens')
import Data.Ix.Functor (IFunctor)

data OrgF k ix where
  OrgObjectF :: OrgObjectData k ObjIx -> OrgF k ObjIx
  OrgElementF :: Keywords (k ObjIx) -> OrgElementData k ElmIx -> OrgF k ElmIx
  OrgSectionF :: OrgSectionData k SecIx -> OrgF k SecIx

secData :: Lens' (OrgF k SecIx) (OrgSectionData k SecIx)
secData f (OrgSectionF d) = OrgSectionF <$> f d

instance TestIxEq (OrgF k) where
  testIxEq SObjIx OrgObjectF {} = Just Refl
  testIxEq SElmIx OrgElementF {} = Just Refl
  testIxEq SSecIx OrgSectionF {} = Just Refl
  testIxEq _ _ = Nothing

deriving instance (AllOrgIx Eq k) => (Eq (OrgF k a))
deriving instance (AllOrgIx Ord k) => (Ord (OrgF k a))
deriving instance (AllOrgIx Show k) => (Show (OrgF k a))
instance (AllOrgIx NFData k) => NFData (OrgF k a) where
  rnf (OrgObjectF x) = rnf x
  rnf (OrgElementF x y) = rnf x `seq` rnf y
  rnf (OrgSectionF x) = rnf x

$(deriveGenericK ''OrgF)
deriving via (Generically OrgF) instance (Endofunctor (~>) OrgF)
deriving via (Generically OrgF) instance (IFoldable OrgF)
deriving via (Generically OrgF) instance (ITraversable OrgF)

newtype OrgNodes f k i = OrgNodes (f (OrgF k) i)
  deriving (Generic)

deriving instance (Eq (f (OrgF k) i)) => Eq (OrgNodes f k i)
deriving instance (Ord (f (OrgF k) i)) => Ord (OrgNodes f k i)
deriving instance (Show (f (OrgF k) i)) => Show (OrgNodes f k i)
deriving newtype instance (NFData (f (OrgF k) i)) => NFData (OrgNodes f k i)
deriving newtype instance (Semigroup (f (OrgF k) i)) => Semigroup (OrgNodes f k i)
deriving newtype instance (Monoid (f (OrgF k) i)) => Monoid (OrgNodes f k i)

$(deriveGenericK ''OrgNodes)
deriving via (Generically (OrgNodes f)) instance (IFunctor f) => (Endofunctor (~>) (OrgNodes f))
deriving via (Generically (OrgNodes f)) instance (IFoldable f) => (IFoldable (OrgNodes f))
deriving via (Generically (OrgNodes f)) instance (ITraversable f) => (ITraversable (OrgNodes f))

type Org f = Fix (OrgNodes f)
{-# COMPLETE Org #-}

pattern Org :: f (OrgF (Org f)) ix -> Org f ix
pattern Org x = Fix (OrgNodes x)
