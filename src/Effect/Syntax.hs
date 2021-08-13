module Effect.Syntax
( -- * Syntax
  Syn(..)
  -- * Connectives
  -- ** With
, type (&)(..)
  -- ** Sum
, type (⊕)(..)
  -- * Continuations
, type (•)(..)
) where

import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Functor.Contravariant
import Data.Traversable

-- Syntax

class Syn rep where
  hdl :: (msg -> rep a) -> rep a


-- Connectives

-- With

newtype a & b = With { getWith :: forall r . Either (r • a) (r • b) -> r }

instance Foldable ((&) a) where
  foldMap = bifoldMap (const mempty)

instance Functor ((&) a) where
  fmap = second

instance Traversable ((&) a) where
  traverse = bitraverse pure

instance Bifoldable (&) where
  bifoldMap = bifoldMapDefault

instance Bifunctor (&) where
  bimap = bimapDefault

instance Bitraversable (&) where
  bitraverse f g r = (\ a b -> With (either (• a) (• b))) <$> getWith r (Left (K f)) <*> getWith r (Right (K g))


-- Sum

data a ⊕ b = L !a | R !b

instance Foldable ((⊕) a) where
  foldMap = foldMapDefault

instance Functor ((⊕) a) where
  fmap = fmapDefault

instance Traversable ((⊕) a)  where
  traverse = bitraverse pure

instance Bifoldable (⊕) where
  bifoldMap = bifoldMapDefault

instance Bifunctor (⊕) where
  bimap = bimapDefault

instance Bitraversable (⊕) where
  bitraverse f g = \case
    L a -> L <$> f a
    R b -> R <$> g b


-- Continuations

newtype r • a = K { (•) :: a -> r }

instance Contravariant ((•) r) where
  contramap f (K g) = K (g . f)
