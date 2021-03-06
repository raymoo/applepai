{-|
Module      : Mahjong.Group.Wait
Description : Defines group waits and some operations involving them
Copyright   : (c) Leon Medvinsky, 2014

License     : GPL-3
Maintainer  : lmedvinsky@hotmail.com
Stability   : experimental
Portability : portable
-}

module Mahjong.Group.Wait (
                            -- * Data
                            Wait(..)
                            -- * Interaction with Groups
                          , waitToGroup
                          , shanponCheck
                          , tankiCheck
                            -- * Lenses
                          , waitTiles
                            -- * Parsers
                          , ryanParser
                          , kanchParser
                          , penParser
                          , shanParser
                          , tanParser
                          , waitParser
                          ) where

import           Control.Applicative
import           Control.Lens
import           Mahjong.Group
import           Mahjong.Tile
import           Parse.MultiSet

-- | Possible waits. It is in general dangerous to use these constructors
-- to construct waits - only do that if you absolutely need to, and are sure
-- it is valid.
data Wait = Ryanmen Tile Tile
          | Kanchan Tile Tile
          | Penchan Tile Tile
          | Shanpon (Tile, Tile) (Tile, Tile)
          | Tanki Tile
            deriving (Show, Eq)

-- | See if you can make a 'Group' with this 'Tile' and a 'Wait'
--
-- Grouping tiles with waits have unique solutions. When in a Shanpon wait, you
-- will get a koutsu and a pair out.
waitToGroup :: Tile -> Wait -> Maybe Group
waitToGroup t (Ryanmen t1 t2)
  | t == (t1 & tileNum %~ pred) = pure $ Shun t t1 t2
  | t == (t2 & tileNum %~ succ) = pure $ Shun t1 t2 t
waitToGroup t (Kanchan t1 t2)
  | t == (t1 & tileNum %~ succ) = pure $ Shun t1 t t2
waitToGroup t (Penchan t1 t2)
  | onTheLeft  && t == (t2 & tileNum %~ succ) = pure $ Shun t1 t2 t
  | onTheRight && t == (t1 & tileNum %~ pred) = pure $ Shun t t1 t2
  where onTheLeft = t1 ^? tileNum == Just One
        onTheRight = not onTheLeft
waitToGroup _ _ = Nothing

-- | Like 'waitToGroup', but checks shanpon and sees if the 'Tile' completes it
--
-- If it works, the second field will always be a koutsu
shanponCheck :: Tile -> Wait -> Maybe (Atama, Group)
shanponCheck t (Shanpon (t11, t12) (t21, t22))
  | t == t11  = Just $ (Atama t21 t22, Kou t t11 t12)
  | t == t21  = Just $ (Atama t11 t12, Kou t t21 t22)
shanponCheck _ _ = Nothing

-- | Like 'shanponCheck and 'waitToGroup', for atama and tanki waits
tankiCheck :: Tile -> Wait -> Maybe Atama
tankiCheck t (Tanki t1)
  | t == t1 = pure $ Atama t t1
tankiCheck _ _ = Nothing

-- | Fold that gets the tiles in a 'Wait'
waitTiles :: Fold Wait Tile
waitTiles f (Ryanmen t1 t2) = Ryanmen <$> f t1 <*> f t2
waitTiles f (Kanchan t1 t2) = Kanchan <$> f t1 <*> f t2
waitTiles f (Penchan t1 t2) = Penchan <$> f t1 <*> f t2
waitTiles f (Shanpon (t11, t12) (t21, t22)) =
                let p1 = (,) <$> f t11 <*> f t12
                    p2 = (,) <$> f t21 <*> f t22
                in Shanpon <$> p1 <*> p2
waitTiles f (Tanki t) = Tanki <$> f t

-- | Helper function to see if a 'Tile' is below a number
inNumRange :: (TNumber, TNumber) -> Tile -> Bool
inNumRange (minN, maxN) =
  maybe False (\n' -> n' <= maxN && n' >= minN) . getTileNum

-- | Helper function for exact numbers
numIs :: TNumber -> Tile -> Bool
numIs n = inNumRange (n,n)

-- | For 'Ryanmen'
ryanParser :: MultiParser Tile Wait
ryanParser = do
  leftSide  <- satisfy $ inNumRange (Two, Seven)
  rightSide <- matchElem $ nextTile leftSide
  return $ Ryanmen leftSide rightSide
  where nextTile t = t & tileNum %~ succ

-- | For 'Kanchan'
kanchParser :: MultiParser Tile Wait
kanchParser = do
  leftSide  <- satisfy $ inNumRange (One, Seven)
  rightSide <- matchElem $ nextTile leftSide
  return $ Kanchan leftSide rightSide
  where nextTile t = t & tileNum %~ succ . succ

-- | For 'Penchan'
penParser :: MultiParser Tile Wait
penParser = do
  leftSide  <- satisfy (numIs One) <|> satisfy (numIs Eight)
  rightSide <- matchElem $ nextTile leftSide
  return $ Penchan leftSide rightSide
  where nextTile t = t & tileNum %~ succ

-- | For 'Shanpon'
shanParser :: MultiParser Tile Wait
shanParser = Shanpon <$> aPair <*> aPair
  where aPair = do
          theFirst <- satisfy $ const True
          theSecond <- matchElem theFirst
          return (theFirst, theSecond)

-- | For 'Tanki'
tanParser :: MultiParser Tile Wait
tanParser = Tanki <$> satisfy (const True)

waitParser :: MultiParser Tile Wait
waitParser = ryanParser <|> kanchParser <|> penParser <|> shanParser <|> tanParser
