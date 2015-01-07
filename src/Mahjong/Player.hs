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
import Control.Lens
import Data.Monoid (Sum(..))
import Data.List   (intercalate)
import qualified Data.MultiSet      as MS
import qualified Data.IntMap.Strict as IM

data Seat = East
          | South
          | West
          | North
          deriving (Eq, Ord, Enum, Bounded, Show)


-- | Represents a hand of tiles a player might have
data Hand =
  Hand { _newTile      :: Maybe Tile      -- ^ newly-drawn 'Tile'
       , _closedTiles  :: IM.IntMap Tile  -- ^ Normal tiles
       , _openGroups   :: [(Group, Seat)] -- ^ Open groups + the seat they got the tile from
       , _closedGroups :: [Group]         -- ^ Closed groups (ie kan)
       }
makeLenses ''Hand

instance Show Hand where
  show h =  (intercalate " " (h^..closedTiles.traversed.to show) ++
             " | " ++
             maybe "" show (h^.newTile) ++
             " | " ++
             intercalate " " (h^..openGroups.traversed._1.to show) ++
             " | " ++
             intercalate " " (h^..closedGroups.traversed.to show))


data Player = Player { _hand  :: Hand
                     , _wind  :: Seat
                     , _river :: MS.MultiSet Tile
                     } deriving (Show)

makeLenses ''Player

getHandTiles :: Hand -> [Tile]
getHandTiles h = h^..handTiles

-- | 'Fold' for the 'Tile's in a hand
handTiles :: Fold Hand Tile
handTiles = folding (\h -> h^..closedTiles.traversed ++
                           h^..openGroups.traversed._1.groupTiles ++
                           h^..closedGroups.traversed.groupTiles ++
                           h^..newTile.traversed
                           )

-- | Tests if the hand has the right number of tiles
wellFormed :: Hand -> Bool
wellFormed h = cTNum + oGNum + cGNum == (12 :: Int)
  where cTNum = countOf (closedTiles.folded) h
        oGNum = countOf (openGroups.traversed._1.groupTiles) h
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
testHand = Hand { _newTile = Just $ Wind E
                , _closedTiles = IM.fromList $ [1..] `zip` replicate 12 (NumT Sou Six)
                , _openGroups  = []
                , _closedGroups = []
                } 

testPlayer :: Player
testPlayer = Player { _hand  = testHand
                    , _wind  = South
                    , _river = MS.fromList $ [NumT Sou Seven, NumT Sou Eight]
                    }
