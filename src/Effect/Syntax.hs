module Effect.Syntax
( -- * Syntax
  Syn(..)
  -- * Connectives
, type (&)(..)
, type (⊕)(..)
  -- * Continuations
, type (•)(..)
) where

import Data.Bifunctor
import Data.Functor.Contravariant

-- Syntax

class Syn rep where
  hdl :: (msg -> rep a) -> rep a


-- Connectives

newtype a & b = With { getWith :: forall x . Either (a -> x) (b -> x) -> x }

instance Bifunctor (&) where
  bimap f g r = With (getWith r . bimap (. f) (. g))

data a ⊕ b = L a | R b

instance Bifunctor (⊕) where
  bimap f g = \case
    L a -> L (f a)
    R b -> R (g b)


-- Continuations

newtype r • a = K { (•) :: a -> r }

instance Contravariant ((•) r) where
  contramap f (K g) = K (g . f)
