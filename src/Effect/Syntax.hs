module Effect.Syntax
( -- * Syntax
  Syn(..)
  -- * Connectives
, type (&)(..)
, type (⊕)(..)
  -- * Continuations
, type (•)(..)
) where

import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Functor.Contravariant

-- Syntax

class Syn rep where
  hdl :: (msg -> rep a) -> rep a


-- Connectives

newtype a & b = With { getWith :: forall r . Either (r • a) (r • b) -> r }

instance Bifunctor (&) where
  bimap f g r = With (getWith r . bimap (contramap f) (contramap g))


data a ⊕ b = L !a | R !b

instance Foldable ((⊕) a) where
  foldMap = bifoldMap (const mempty)

instance Functor ((⊕) a) where
  fmap = second

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
