{-# LANGUAGE TemplateHaskell #-}

module Mahjong.Player (
                        Seat
                        -- * Hands
                      , Hand(..)
                      , closedTiles
                      , openGroups
                      , closedGroups
                      , getHandTiles
                      , handTiles
                      , wellFormed
                        -- * Players
                      , Player(..)
                      , hand
                      , wind
                      , river
                      , discard
                        -- * Debug
                      , testHand
                      , testPlayer
                      ) where

import Mahjong.Group
import Mahjong.Tile
import Parse.MultiSet
import Control.Lens
import Control.Applicative((<$>))
import Data.Monoid (Sum(..))
import qualified Data.MultiSet      as MS
import qualified Data.IntMap.Strict as IM

data Seat = East
          | South
          | West
          | North
          deriving (Eq, Ord, Enum, Bounded, Show)

data Player = Player { _hand  :: Hand
                     , _wind  :: Seat
                     , _river :: MS.MultiSet Tile
                     } deriving (Show)

-- | Represents a hand of tiles a player might have
data Hand =
  Hand { _newTile      :: Maybe Tile      -- ^ newly-drawn 'Tile'
       , _closedTiles  :: IM.IntMap Tile  -- ^ Normal tiles
       , _openGroups   :: [Group]         -- ^ Open groups
       , _closedGroups :: [Group]         -- ^ Closed groups (ie kan)
       } deriving (Show)

makeLenses ''Hand
makeLenses ''Player

getHandTiles :: Hand -> [Tile]
getHandTiles h = h^..handTiles

-- | 'Fold' for the 'Tile's in a hand
handTiles :: Fold Hand Tile
handTiles = folding (\h -> h^..closedTiles.traversed ++
                           h^..openGroups.traversed.groupTiles ++
                           h^..closedGroups.traversed.groupTiles ++
                           h^..newTile.traversed
                           )

-- | Tests if the hand has the right number of tiles
wellFormed :: Hand -> Bool
wellFormed h = cTNum + oGNum + cGNum == (12 :: Int)
  where cTNum = countOf (closedTiles.folded) h
        oGNum = countOf (openGroups.traversed.groupTiles) h
        cGNum = countOf (closedGroups.traversed.groupTiles) h
        countOf l = getSum . foldMapOf l (const (Sum 1))

-- | Discard the tile on that index. Nothing means to discard the drawn tile
discard :: Maybe Int -> Player -> Maybe Player
discard _       Player{ _hand = Hand{_newTile = Nothing} } = Nothing
discard Nothing   p                          = Just $ p & hand.newTile .~ Nothing
                                                        & river %~ MS.insert (p^?!hand.newTile.traversed)
discard (Just i)  p = do
  old <- IM.lookup i (p^.hand.closedTiles)
  return $ p & river %~ MS.insert old
             & hand.closedTiles %~ IM.insert i (p^?!hand.newTile.traversed)
             & hand.newTile .~ Nothing

testHand :: Hand
testHand = Hand { _newTile = Just E
                , _closedTiles = IM.fromList $ [1..] `zip` replicate 12 (Sou Six)
                , _openGroups  = []
                , _closedGroups = []
                } 

testPlayer :: Player
testPlayer = Player { _hand  = testHand
                    , _wind  = South
                    , _river = MS.empty
                    }
