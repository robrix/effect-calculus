module Effect.Syntax
( Syn(..)
  -- * Connectives
, type (&)(..)
  -- * Continuations
, type (•)(..)
) where

class Syn rep where
  hdl :: (msg -> rep a) -> rep a


-- Connectives

newtype a & b = With { getWith :: forall x . (Either (a -> x) (b -> x) -> x) -> x }


-- Continuations

newtype r • a = K { (•) :: a -> r }
