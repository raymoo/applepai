{-|
Module      : Mahjong.Cycle
Description : Defines the typeclass Cycle, which captures the pattern of things
              that can wrap around
Copyright   : (c) Leon Medvinsky, 2014

License     : GPL-3
Maintainer  : lmedvinsky@hotmail.com
Stability   : experimental
Portability : portable
-}

module Mahjong.Cycle (Cycle(..)) where

-- | Class for things you can enumerate, but which wrap around instead of having
-- bounds.
class Cycle a where
  forward :: a -> a
  backward :: a -> a

instance Cycle () where
  forward = id
  backward = id

instance Cycle Bool where
  forward = not
  backward = not
