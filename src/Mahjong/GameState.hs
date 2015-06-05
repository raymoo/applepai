{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Mahjong.GameState (
                            -- * Players
                           PlayerNum(..)
                         , Players(..)
                         , p1
                         , p2
                         , p3
                         , p4
                           -- * The Wall
                         , TileWall(..)
                         , twTiles
                         , twCount
                         , -- * Game State
                         , GameState(..)
                           -- ** Generic State
                         , GenState(..)
                         , gsWall
                         , gsPlayers
                         , gsSticks
                           -- ** Moves
                         , GameMove(..)
                         ) where

import Mahjong.Player
import Control.Lens

data PlayerNum = P1 | P2 | P3 | P4
               deriving (Show, Eq, Ord, Enum, Bounded)

data Players =
  Players { _p1 :: Player
          , _p2 :: Player
          , _p3 :: Player
          , _p4 :: Player
          }
  deriving (Show)

makeLenses ''Players

data TileWall =
  TileWall { _twRawTiles :: [Tile]
           , _twRawCount :: Int
           }

makeLenses ''TileWall

-- | 'Getter' for the tiles in a wall
twTiles :: Getter' TileWall [Tile]
twTiles = twRawTiles

-- | 'Getter' for the number of tiles in a wall
twCount :: Getter' TileWall Int
twCount = twRawCount

-- | Generic GameState - This information is common to all game states.
data GenState =
  GenState { _gsPlayers :: Players -- The players in the game
           , _gsWall    :: TileWall  -- The wall
           , _gsSticks  :: }
  deriving (Show)

makeLenses ''GenState

player :: PlayerNum -> IndexedLens' PlayerNum Players Player
player num pafb = pLens aToFa
  where aToFa = indexed pafb num
        pLens = case num of
                 P1 -> p1
                 P2 -> p2
                 P3 -> p3
                 P4 -> p4

data GameState = GSNew  -- ^ The Game has just begun
               | GSDraw -- ^ Someone has just drawn
               | GSDC   -- ^ Someone has just discarded
               | GSWin  -- ^ Someone just won

data GameMove
