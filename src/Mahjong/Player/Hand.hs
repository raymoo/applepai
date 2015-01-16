{-|
Module      : Mahjong.Player.Hand
Description : Describes the tiles a player holds
Copyright   : (c) Leon Medvinsky, 2014

License     : GPL-3
Maintainer  : lmedvinsky@hotmail.com
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE TemplateHaskell #-}

module Mahjong.Player.Hand (
              -- * Hands
              Hand(..)
              -- ** Query
            , getHandTiles
            , wellFormed
              -- ** Lenses
            , newTile
            , closedTiles
            , openGroups
            , closedGroups
            , handTiles
              -- * Results
            , Result(..)
              -- ** Lenses
            , resWait
            , resTsumo
            , resOpens
            , resCloseds
              -- * Testing
            , testHand
            ) where

import           Control.Lens
import           Control.Applicative
import qualified Data.IntMap.Strict as IM
import           Data.List          (intercalate)
import           Data.Monoid
import           Mahjong.Group
import           Mahjong.Group.Wait
import           Mahjong.Tile
import           Parse.MultiSet

type Seat = Direction

-- | Represents a hand of tiles a player might have
data Hand =
  Hand { _newTile      :: Maybe Tile      -- ^ newly-drawn 'Tile'
       , _closedTiles  :: IM.IntMap Tile  -- ^ Normal tiles
       , _openGroups   :: [(Group, Seat)] -- ^ Open groups + the seat they got the tile from
       , _closedGroups :: [Group]         -- ^ Closed groups (ie kan)
       }
makeLenses ''Hand

-- | Gets the tiles in a hand
getHandTiles :: Hand -> [Tile]
getHandTiles h = h^..handTiles

-- | 'Fold' for the 'Tile's in a hand
handTiles :: Fold Hand Tile
handTiles = folding (\h -> h^..closedTiles.traversed ++
                           h^..openGroups.traversed._1.groupTiles ++
                           h^..closedGroups.traversed.groupTiles ++
                           h^..newTile.traversed
                           )


instance Show Hand where
  show h =  (intercalate " " (h^..closedTiles.traversed.to show) ++
             " | " ++
             maybe "" show (h^.newTile) ++
             " | " ++
             intercalate " " (h^..openGroups.traversed._1.to show) ++
             " | " ++
             intercalate " " (h^..closedGroups.traversed.to show))

-- | Tests if the hand has the right number of tiles
wellFormed :: Hand -> Bool
wellFormed h = cTNum + oGNum + cGNum == (12 :: Int)
  where cTNum = countOf (closedTiles.folded) h
        oGNum = countOf (openGroups.traversed._1.groupTiles) h
        cGNum = countOf (closedGroups.traversed.groupTiles) h
        countOf l = getSum . foldMapOf l (const (Sum 1))

testHand :: Hand
testHand =
  Hand { _newTile = Just $ Wind E
       , _closedTiles = IM.fromList $ [1..] `zip` replicate 12 (NumT Sou Six)
       , _openGroups  = []
       , _closedGroups = []
       }

data Result =
  Result { _resWait    :: Wait
         , _resTsumo   :: Tile
         , _resOpens   :: [Group]
         , _resCloseds :: [Group]
         }

makeLenses ''Result

-- | Try to create valid 'Result's from this 'Hand'.
tryAgari :: Hand -> [Result]
tryAgari hand = undefined
  where threeParser = kouParser <|> shunParser
        threeWParser = ryanParser <|> kanchParser <|> penParser
        shanHandP = (,) <$> count 3 threeParser <*> shanParser
        tankiHandP = (,) <$> count 4 threeParser <*> tanParser
        normHandP =
          (,,) <$> count 3 threeParser <*> atamaParser <*> threeWParser
