module Effect.Syntax
( -- * Syntax
  Syn(..)
  -- * Connectives
  -- ** With
, (&)
, type (&)(..)
  -- ** Sum
, type (⊕)(..)
  -- * Continuations
, type (•)(..)
  -- * Values
, (%)
, type (%)(..)
  -- * Conjunctions
, Conj(..)
  -- * Disjunctions
, Disj(..)
) where

import Control.Applicative (liftA2)
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

newtype a & b = With { getWith :: forall r . r • Either (r • a) (r • b) }

infixr 6 &

instance Conj (&) where
  (>-<) = liftA2 (\ a b -> With (K (either (• a) (• b))))
  exl a = K (\ (With k) -> k • Left a)
  exr b = K (\ (With k) -> k • Right b)

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
  bitraverse f g w = (&) <$> exl (K f) • w <*> exr (K g) • w


-- Sum

data a ⊕ b = L !a | R !b

infixr 6 ⊕

instance Disj (⊕) where
  inl = fmap L
  inr = fmap R
  l <-> r = K (\case
    L a -> l • a
    R b -> r • b)

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


-- Values

(%) :: e -> e % a -> a
a % V f = f a

newtype e % a = V (e -> a)
  deriving (Applicative, Functor, Monad)

infixl 9 %


-- Conjunctions

class Conj c where
  (>-<) :: Applicative f => f a -> f b -> f (a `c` b)
  infixr 4 >-<
  exl :: (r • a) -> r • (a `c` b)
  exr :: (r • b) -> r • (a `c` b)


-- Disjunctions

class Disj d where
  inl :: Functor f => f a -> f (a `d` b)
  inr :: Functor f => f b -> f (a `d` b)
  (<->) :: (r • a) -> (r • b) -> r • (a `d` b)
  infixr 3 <->
