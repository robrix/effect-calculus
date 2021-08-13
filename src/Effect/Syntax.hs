module Effect.Syntax
( -- * Syntax
  Syn(..)
  -- * Connectives
  -- ** With
, (&)
, exl
, exr
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

(&) :: a -> b -> a & b
a & b = With (K (either (• a) (• b)))

exl :: a & b -> a
exl (With k) = k • Left (K id)

exr :: a & b -> b
exr (With k) = k • Right (K id)

newtype a & b = With { getWith :: forall r . r • Either (r • a) (r • b) }

infixr 6 &

instance Foldable ((&) a) where
  foldMap = foldMapDefault

instance Functor ((&) a) where
  fmap = fmapDefault

instance Traversable ((&) a) where
  traverse = bitraverse pure

instance Bifoldable (&) where
  bifoldMap = bifoldMapDefault

instance Bifunctor (&) where
  bimap = bimapDefault

instance Bitraversable (&) where
  bitraverse f g w = (&) <$> f (exl w) <*> g (exr w)


-- Sum

data a ⊕ b = L !a | R !b

infixr 6 ⊕

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

infixl 8 •

instance Contravariant ((•) r) where
  contramap f (K g) = K (g . f)
