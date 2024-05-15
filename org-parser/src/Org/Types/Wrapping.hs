{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | How to handle onions without tears. 🧅
module Org.Types.Wrapping where
import Data.Ix.Traversable (ITraversable, itraverse)
import Data.Ix.RecursionSchemes (Fix (..))
import Control.Category.Natural (type (~~>))

type ITraversal w t = forall f. Applicative f => (forall i. t i -> f (t i)) -> (forall i. w i -> f (w i))

-- | Like traversable but with specified types.
class Wrapper w t | w -> t where
  wrapped :: ITraversal w t

instance (ITraversable f) => Wrapper (f t) t where
  wrapped = itraverse

instance (ITraversable f) => Wrapper (Fix f) (f (Fix f)) where
  wrapped f (Fix x) = Fix <$> f x

class HasInside w t where
  insideF :: ITraversal w t

inside :: HasInside w t => (t ~~> t) -> w ~~> w
inside f = runIdentity . insideF (pure . f)

instance {-# OVERLAPPABLE #-} HasInside t t where
  insideF f = f

instance (Wrapper w s, HasInside s t) => HasInside w t where
  insideF f = wrapped $ insideF f
