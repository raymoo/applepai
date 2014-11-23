{-|
Module      : Mahjong.Group
Description : Groups of mahjong tiles
Copyright   : (c) Leon Medvinsky, 2014

License     : GPL-3
Maintainer  : lmedvinsky@hotmail.com
Stability   : experimental
Portability : portable
-}

module Mahjong.Group(
                      -- * Mahjong groups
                      Group
                      -- ** Group types
                    , GroupType(..)
                    , groupType
                      -- ** Constructors
                      -- | These constructors fail when the provided tiles do not
                      -- form a valid group of that type.
                    , atama
                    , shun
                    , kou
                    , kan
                      -- * Query
                    , getGroupTiles
                    , getTileCount
                      -- * Parsing
                    , kouParser
                    , kanParser
                    , atamaParser
                    , shunParser
                      -- * Lenses
                    , groupTiles
                    , tileCount
 )where

import           Control.Applicative ((<$>), (<*>))
import           Control.Lens
import           Data.Maybe          (isJust)
import qualified Data.MultiSet       as MS
import           Mahjong.Tile
import           Parse.MultiSet

-- | Mahjong groups
data Group = Atama Tile Tile -- ^ Pair
           | Shun Tile Tile Tile
           | Kou Tile Tile Tile
           | Kan Tile Tile Tile Tile
             deriving (Show, Eq, Ord)

-- | The different kinds of tile groups
data GroupType = Pair
               | Shuntsu
               | Koutsu
               | Kantsu

groupType :: Group -> GroupType
groupType (Atama _ _)    = Pair
groupType (Shun _ _ _)   = Shuntsu
groupType (Kou _ _ _)    = Koutsu
groupType (Kan _ _ _ _)  = Kantsu

atama :: Tile -> Tile -> Maybe Group
atama t@(Man n) t2@(Man n2)
    | n == n2 = Just $ Atama t t2
atama t@(Sou n) t2@(Sou n2)
    | n == n2 = Just $ Atama t t2
atama t@(Pin n) t2@(Pin n2)
    | n == n2 = Just $ Atama t t2
atama t t2
    | t == t2 = Just $ Atama t t2
atama _ _ = Nothing

shun :: Tile -> Tile -> Tile -> Maybe Group
shun t t2 t3
    | suitsEqual && successive = Just $ Shun t t2 t3
    | otherwise                = Nothing
    where suit1 = getTileSuit t
          suit2 = getTileSuit t2
          suit3 = getTileSuit t3
          suitsEqual = suit1 == suit2 && suit2 == suit3
          num1 = getTileNum t
          num2 = getTileNum t2
          num3 = getTileNum t3
          notHonors = all isJust [num1, num2, num3]
          successive = notHonors &&
                       num2 == fmap succ num1 &&
                       num3 == fmap succ num2

kou :: Tile -> Tile -> Tile -> Maybe Group
kou t@(Man n) t2@(Man n2) t3@(Man n3)
    | n  == n2 &&
      n2 == n3 = Just $ Kou t t2 t3
kou t@(Sou n) t2@(Sou n2) t3@(Sou n3)
    | n  == n2 &&
      n2 == n3 = Just $ Kou t t2 t3
kou t@(Pin n) t2@(Pin n2) t3@(Pin n3)
    | n  == n2 &&
      n2 == n3 = Just $ Kou t t2 t3
kou t t2 t3
    | t  == t2 &&
      t2 == t3 = Just $ Kou t t2 t3
kou _ _ _ = Nothing

kan :: Tile -> Tile -> Tile -> Tile -> Maybe Group
kan t@(Man n) t2@(Man n2) t3@(Man n3) t4@(Man n4)
    | n  == n2 &&
      n2 == n3 &&
      n3 == n4 = Just $ Kan t t2 t3 t4
kan t@(Sou n) t2@(Sou n2) t3@(Sou n3) t4@(Sou n4)
    | n  == n2 &&
      n2 == n3 &&
      n3 == n4 = Just $ Kan t t2 t3 t4
kan t@(Pin n) t2@(Pin n2) t3@(Pin n3) t4@(Pin n4)
    | n  == n2 &&
      n2 == n3 &&
      n3 == n4 = Just $ Kan t t2 t3 t4
kan t t2 t3 t4
    | t  == t2 &&
      t2 == t3 &&
      t3 == t4 = Just $ Kan t t2 t3 t4
kan _ _ _ _ = Nothing

-- | Gets the 'Tile's in a 'Group'
getGroupTiles :: Group -> [Tile]
getGroupTiles = toListOf groupTiles

-- | Gets the number of 'Tile's in a 'Group'
getTileCount :: Group -> Int
getTileCount (Atama _ _)   = 2
getTileCount (Shun _ _ _)  = 3
getTileCount (Kou _ _ _)   = 3
getTileCount (Kan _ _ _ _) = 4

-- | Parses a Kou
kouParser :: MultiParser Tile Group
kouParser = fmap listToKou $ forValues (nOf 3)
   -- unsafe! Relies on there always being three matching tiles
  where listToKou (t:t':t'':_) = Kou t t' t''

-- | Parses a kan
kanParser :: MultiParser Tile Group
kanParser = fmap listToKan $ forValues (nOf 4)
  -- same as above
  where listToKan (t:t':t'':t''':_) = Kan t t' t'' t'''

-- | Parses a pair
atamaParser :: MultiParser Tile Group
atamaParser = fmap listToAtama $ forValues (nOf 2)
  where listToAtama (t:t':_) = Atama t t'

-- | Parses a Shun
shunParser :: MultiParser Tile Group
shunParser = forFilteredValues checkNum
  where parseSequence t =
          Shun <$> satisfy (==t) <*> t2 <*> t3
          where advance :: Tile -> Int -> MultiParser Tile Tile
                advance t' n = satisfy (\t'' ->
                                         t''^.tileSuit == t'^.tileSuit &&
                                         t''^..tileNum == t'^..tileNum.(to $ succN n))
                t2 = advance t 1
                t3 = advance t 2
        succN 0 x = x
        succN n x = succN (n - 1) (succ x)
        checkNum t = getTileNum t >>= \n -> if n > Seven
                                            then Nothing
                                            else Just $ parseSequence t

-- | A 'Fold' focusing on the 'Tile's of a 'Group'
groupTiles :: Fold Group Tile
groupTiles f (Atama t1 t2)     = Atama <$> f t1 <*> f t2
groupTiles f (Shun t1 t2 t3)   = Shun <$> f t1 <*> f t2 <*> f t3
groupTiles f (Kou t1 t2 t3)    = Kou <$> f t1 <*> f t2 <*> f t3
groupTiles f (Kan t1 t2 t3 t4) = Kan <$> f t1 <*> f t2 <*> f t3 <*> f t4

-- | A 'Getter' focusing on the number of 'Tile's in a 'Group'
tileCount :: Getter Group Int
tileCount = \f s -> coerce $ f $ getTileCount s
