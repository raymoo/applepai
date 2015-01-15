{-|
Module      : Mahjong.Player
Description : Defines players, and some operations on them
Copyright   : (c) Leon Medvinsky, 2014

License     : GPL-3
Maintainer  : lmedvinsky@hotmail.com
Stability   : experimental
Portability : portable
-}

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
                      , testPlayer
                      ) where

import           Control.Lens
import           Mahjong.Tile
import qualified Data.IntMap.Strict as IM
import qualified Data.MultiSet      as MS
import           Mahjong.Player.Hand

type Seat = Direction


data Player = Player { _hand  :: Hand
                     , _wind  :: Seat
                     , _river :: MS.MultiSet Tile
                     } deriving (Show)

makeLenses ''Player


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

testPlayer :: Player
testPlayer = Player { _hand  = testHand
                    , _wind  = S
                    , _river = MS.fromList $ [NumT Sou Seven, NumT Sou Eight]
                    }
