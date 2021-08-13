module Effect.Syntax
( Syn(..)
) where

class Syn rep where
  hdl :: (msg -> rep a) -> rep a
