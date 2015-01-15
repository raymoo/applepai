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
                      Group(..)
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
                      -- * Testing
                    , standardTestHand
 )where

import           Control.Applicative ((<$>), (<*>), (<|>))
import           Control.Lens
import           Data.List           (intercalate)
import           Data.Maybe          (isJust)
import           Mahjong.Tile
import           Parse.MultiSet

-- | Mahjong groups
-- Don't use these constructors to create groups - this is unsafe, since you
-- might make invalid Groups.
data Group = Atama Tile Tile -- ^ Pair
           | Shun Tile Tile Tile
           | Kou Tile Tile Tile
           | Kan Tile Tile Tile Tile
             deriving (Eq, Ord)

instance Show Group where
  show g = intercalate "-" $ g^..groupTiles.(to show)

-- | The different kinds of tile groups
data GroupType = Pair
               | Shuntsu
               | Koutsu
               | Kantsu

groupType :: Group -> GroupType
groupType Atama{} = Pair
groupType Shun{}  = Shuntsu
groupType Kou{}   = Koutsu
groupType Kan{}   = Kantsu

atama :: Tile -> Tile -> Maybe Group
atama t@(NumT s n) t2@(NumT s2 n2)
    | n == n2 &&
      s == s2    = Just $ Atama t t2
atama t t2
    | t == t2 = Just $ Atama t t2
atama _ _ = Nothing

-- | Requires the tiles be in ascending order
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
kou t@(NumT s n) t2@(NumT s2 n2) t3@(NumT s3 n3)
    | n  == n2 &&
      n2 == n3 &&
      s  == s2 &&
      s2 == s3    = Just $ Kou t t2 t3
kou t t2 t3
    | t  == t2 &&
      t2 == t3 = Just $ Kou t t2 t3
kou _ _ _ = Nothing

kan :: Tile -> Tile -> Tile -> Tile -> Maybe Group
kan t@(NumT s n) t2@(NumT s2 n2) t3@(NumT s3 n3) t4@(NumT s4 n4)
    | n  == n2 &&
      n2 == n3 &&
      n3 == n4 &&
      s  == s2 &&
      s2 == s3 &&
      s3 == s4    = Just $ Kan t t2 t3 t4
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
getTileCount Atama{} = 2
getTileCount Shun{}  = 3
getTileCount Kou{}   = 3
getTileCount Kan{}   = 4

-- | Parses a Kou
kouParser :: MultiParser Tile Group
kouParser = listToKou <$> forValues (nOf 3)
   -- unsafe! Relies on there always being three matching tiles
  where listToKou (t:t':t'':_) = Kou t t' t''
        listToKou _            = error "listToKou failed"

-- | Parses a kan
kanParser :: MultiParser Tile Group
kanParser = listToKan <$> forValues (nOf 4)
  -- same as above
  where listToKan (t:t':t'':t''':_) = Kan t t' t'' t'''
        listToKan _                 = error "listToKan failed"

-- | Parses a pair
atamaParser :: MultiParser Tile Group
atamaParser = listToAtama <$> forValues (nOf 2)
  where listToAtama (t:t':_) = Atama t t'
        listToAtama _        = error "listToAtama failed"

-- | Parses a Shun
shunParser :: MultiParser Tile Group
shunParser = forFilteredValues checkNum
  where parseSequence t =
          Shun <$> satisfy (==t) <*> t2 <*> t3
          where advance :: Tile -> Int -> MultiParser Tile Tile
                advance t' n = satisfy (\t'' ->
                                         t''^.tileSuit == t'^.tileSuit &&
                                         t''^..tileNum == t'^..tileNum.to (succN n))
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


-- | "Standard" hand. For testing only, since a real hand might already have
-- open groups.
standardTestHand :: MultiParser Tile [Group]
standardTestHand = getEachOf [(1,atamaParser), (4, threeParser)]
  where threeParser = kouParser <|> shunParser

