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
                         , twDeadTiles
                           -- * Game State
                         , GameState(..)
                           -- ** Generic State
                         , GenState(..)
                         , gsWall
                         , gsPlayers
                         , gsSticks
                           -- ** Moves
                         , GameMove(..)
                           -- *** Validation
                         , validateMove
                         ) where

import Data.Maybe

import Mahjong.Player
import Control.Lens
import Mahjong.Tile
import Mahjong.Player.Hand
import Mahjong.Group
import Parse.MultiSet
import qualified Data.MultiSet as MS

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

data DeadWall 

data TileWall =
  TileWall { _twRawTiles     :: [Tile]
           , _twRawDeadTiles :: [Tile]
           , _twRawCount     :: Int
           }
  deriving (Show)

makeLenses ''TileWall

-- | 'Getter' for the tiles in a wall
twTiles :: Getter TileWall [Tile]
twTiles = twRawTiles

-- | 'Getter' for the number of tiles in a wall
twCount :: Getter TileWall Int
twCount = twRawCount

-- | 'Getter' for the tiles in the dead wall
twDeadTiles :: Getter TileWall [Tile]
twDeadTiles = twRawDeadTiles

-- | Generic GameState - This information is common to all game states.
data GenState =
  GenState { _gsPlayers :: Players -- The players in the game
           , _gsWall    :: TileWall  -- The wall
           , _gsSticks  :: Int
           }
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

data GameState =
    GSNew GenState PlayerNum -- ^ The Game has just begun
  | GSDraw GenState PlayerNum -- ^ Someone has just drawn
  | GSDC GenState PlayerNum Tile -- ^ Someone has just discarded
  | GSWin  -- ^ Someone just won

data GameMove =
    Nop
  | Discard (Maybe Int)
  | GMKan Tile
  | Win WinMethod
  | Call


validateMove :: GameState -> PlayerNum -> GameMove -> Bool
validateMove (GSNew gs pcurr) pn Nop         = pn /= pcurr
validateMove (GSNew gs pcurr) pn (Discard i)
  | pn /= pcurr = False
  | otherwise = isJust $ discard i (gs^.gsPlayers.player pn)
validateMove (GSNew gs pcurr) pn (GMKan t)
  | pn /= pcurr = False
  | otherwise =
      let tiles = gs^..gsPlayers.player pn.hand.handTiles
          kanRes = runParser (nOf 4 t) (MS.fromList tiles)
      in not . null $ kanRes
validateMove (GSNew gs pcurr) pn (Win wm)
  | pn /= pcurr = False
  | otherwise =
      case wm of
       MRon _ _ -> False
       -- TODO: Check yaku count maybe? In case tenhou rule not being used
       MTsumo   -> let agariRes = tryAgari wm (gs^.gsPlayers.player pn.hand)
                   in not . null $ agariRes
validateMove (GSNew _ _) _ Call    = False

validateMove (GSDraw gs pcurr) pn Nop         = pn /= pcurr
validateMove (GSDraw gs pcurr) pn (Discard i)
  | pn /= pcurr = False
  | otherwise = isJust $ discard i (gs^.gsPlayers.player pn)
validateMove (GSDraw gs pcurr) pn (GMKan t)
  | pn /= pcurr = False
  | otherwise =
      let tiles = gs^..gsPlayers.player pn.hand.handTiles
          kanRes = runParser kanParser (MS.fromList tiles)
      in not . null $ kanRes
validateMove (GSDraw gs pcurr) pn (Win wm)
  | pn /= pcurr = False
  | otherwise =
      case wm of
       MRon _ _ -> False
       -- TODO: Check yaku count
       MTsumo   -> let agariRes = tryAgari wm (gs^.gsPlayers.player pn.hand)
                   in not . null $ agariRes
validateMove (GSDraw _ _) _ Call    = False

validateMove (GSDC _ _ _) _ Nop       = True
validateMove (GSDC _ _ _) _ Discard{} = False
validateMove (GSDC _ _ _) _ GMKan{}   = False
validateMove (GSDC gs pcurr dTile) pn (Win wm)
  | pn == pcurr = False
  | otherwise = case wm of
                 MTsumo -> False
                 -- TODO: Check previously discarded tiles
                 MRon tile donator ->
                   let agariRes = tryAgari wm (gs^.gsPlayers.player pn.hand)
                   in not . null $ agariRes
validateMove (GSDC gs pcurr dTile) pn Call
  | pn == pcurr = False
  | otherwise = undefined -- TODO

validateMove (
  
