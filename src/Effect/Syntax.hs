{-# LANGUAGE FunctionalDependencies #-}
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
  -- * Contravariant applicative
, comap
, Contrapply(..)
, Contrapplicative(..)
  -- * Functions
, Fun(..)
  -- ** Mixfix syntax
, type (~~)
, type (~>)
  -- * Cofunctions
, Cofun(..)
  -- ** Mixfix syntax
, type (>-)
, type (-~)
  -- ** Construction
, (>-)
  -- ** Computation
, cocurry
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
  exl a = contramap (\ (With k) -> k • Left  (K id)) a
  exr b = contramap (\ (With k) -> k • Right (K id)) b

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
    L a -> l • a
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

instance Contrapply r ((•) r) where
  coliftC2 f a b = K (\ c -> a • f (b :>- c))
  K f <&> a = K (\ b -> f (a :>- b))

instance Contrapplicative r ((•) r) where
  copure f = K (\ (a :>- b) -> a • f b)


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
  exl :: Contravariant k => k a -> k (a `c` b)
  exr :: Contravariant k => k b -> k (a `c` b)

instance Conj (,) where
  (>-<) = liftA2 (,)
  exl = comap fst
  exr = comap snd


-- Disjunctions

class Disj d where
  inl :: Functor f => f a -> f (a `d` b)
  inr :: Functor f => f b -> f (a `d` b)
  (<->) :: (r • a) -> (r • b) -> r • (a `d` b)
  infixr 3 <->

instance Disj Either where
  inl = fmap Left
  inr = fmap Right
  f <-> g = K (either (f •) (g •))


-- Contravariant applicative

comap :: Contravariant f => (a' -> a) -> (f a -> f a')
comap = contramap


class Contravariant k => Contrapply r k | k -> r where
  {-# MINIMAL coliftC2 | (<&>) #-}

  coliftC2 :: ((b >-r-~ c) -> a) -> k a -> k b -> k c
  coliftC2 f = (<&>) . comap f

  (<&>) :: k (a >-r-~ b) -> k a -> k b
  (<&>) = coliftC2 id

  infixl 4 <&>

instance Contrapply Bool Predicate where
  f <&> a = Predicate (getPredicate f . (K (getPredicate a) >-))


class Contrapply r k => Contrapplicative r k | k -> r where
  copure :: (b -> a) -> k (a >-r-~ b)

instance Contrapplicative Bool Predicate where
  copure f = Predicate (\ (a :>- b) -> a • f b)


-- Functions

newtype Fun r a b = Fun { getFun :: (r • b) -> (r • a) }


-- Mixfix syntax

type a ~~r = Fun r a
type r~> b = r b

infixr 1 ~~
infixr 0 ~>


-- Cofunctions

data Cofun r b a = (:>-) { coreturn :: r • b, coconst :: a }


-- Mixfix syntax

type a >-r = Cofun r a
type r-~ b = r b

infixr 1 >-
infixr 0 -~


-- Construction

(>-) :: (r • b) -> a -> Cofun r b a
(>-) = (:>-)


-- Computation

cocurry :: (c -> Either a b) -> Fun r (Cofun r b c) a
cocurry f = Fun (\ k -> K (\ (b :>- c) -> (k <-> b) • f c))
