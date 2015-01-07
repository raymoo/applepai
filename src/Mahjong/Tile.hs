{-|
Module      : Mahjong.Tile
Description : Defines mahjong tiles and some functions on them
Copyright   : (c) Leon Medvinsky, 2014

License     : GPL-3
Maintainer  : lmedvinsky@hotmail.com
Stability   : experimental
Portability : portable
-}

module Mahjong.Tile (
                      -- * Types
                      Tile(..)
                    , TNumber(..)
                    , TSuit(..)
                      -- * Tile information
                    , getTileSuit
                    , getTileNum
                      -- * Generation
                    , getSuitTiles
                    , getNumTiles
                      -- * Convenience
                    , everyTile
                      -- * Lenses
                    , tileNum
                    , maybeTileNum
                    , tileSuit
                    , suitTiles

 )where

import           Control.Applicative (Applicative, (<$>), pure)
import           Control.Lens


-- | Mahjong tile values

data Tile = -- | Manzu
            Man TNumber
            -- | Souzu
          | Sou TNumber
            -- | Pinzu
          | Pin TNumber
            -- | Dragons
          | DGreen | DRed | DWhite
            -- | Winds
          | E | S | W | N
            deriving (Eq, Ord)

instance Show Tile where
  show (Man n) = 'M' : show n -- [chr $ fromEnum n + 0x1F007]
  show (Sou n) = 'S' : show n -- [chr $ fromEnum n + 0x1F010]
  show (Pin n) = 'P' : show n -- [chr $ fromEnum n + 0x1F019]
  show DGreen  = "發" -- "\x1F005"
  show DRed    = "中" -- "\x1F004"
  show DWhite  = "白" -- "\x1F006"
  show E       = "東" -- "\x1F000"
  show S       = "南" -- "\x1F001"
  show N       = "北" -- "\x1F003"
  show W       = "西" -- "\x1F002"

{-
-- Constants for the Enum instance
souOffset, pinOffset, honOffset :: Int
souOffset = 9
pinOffset = souOffset + 9
honOffset = pinOffset + 9


instance Enum Tile where
    fromEnum (Man n) = fromEnum n
    fromEnum (Sou n) = fromEnum n + souOffset
    fromEnum (Pin n) = fromEnum n + pinOffset
    fromEnum DGreen  = honOffset
    fromEnum DRed    = honOffset + 1
    fromEnum DWhite  = honOffset + 2
    fromEnum E       = honOffset + 3
    fromEnum S       = honOffset + 4
    fromEnum W       = honOffset + 5
    fromEnum N       = honOffset + 6
    toEnum n = case n `div` 9 of
                 0 -> Man numInt
                 1 -> Sou numInt
                 2 -> Pin numInt
                 _ -> toHonor n
        where numInt = toEnum $ n `mod` 9
              toHonor 27 = DGreen
              toHonor 28 = DRed
              toHonor 29 = DWhite
              toHonor 30 = E
              toHonor 31 = S
              toHonor 32 = W
              toHonor 33 = N


instance Bounded Tile where
    minBound = Man One
    maxBound = N
-}

-- | Possible numerical value of tiles ('Honor' for honors)
data TNumber = One | Two | Three | Four | Five | Six | Seven | Eight | Nine
               deriving (Eq, Ord, Enum, Bounded)

instance Show TNumber where
  show = show . (+1) . fromEnum

-- | Tile suits. Honors are split up into two suits: 'Dragon' and 'Wind'
--
-- If you want to check if something is an honor, use 'TNumber's instead.
data TSuit = Manzu
           | Souzu
           | Pinzu
           | Dragon
           | Wind
             deriving (Show, Eq) -- Does it make sense for suits to have an
                                 -- ordering?

-- | Gets the 'TSuit' of a 'Tile'
getTileSuit :: Tile -> TSuit
getTileSuit (Man _) = Manzu
getTileSuit (Sou _) = Souzu
getTileSuit (Pin _) = Pinzu
getTileSuit t
  | t >= DGreen && t <= DWhite = Dragon
  | t >= E && t <= W           = Wind
getTileSuit _ = error "The above definition should be complete"

-- | Gets the numerical value of a tile
getTileNum :: Tile -> Maybe TNumber
getTileNum (Man n) = Just n
getTileNum (Sou n) = Just n
getTileNum (Pin n) = Just n
getTileNum _       = Nothing

-- | Find all tiles matching a 'TSuit'
getSuitTiles :: TSuit -> [Tile]
getSuitTiles s = filter ((== s) . getTileSuit) everyTile

-- | Make a numerical tile from 'TSuit' and 'TNumber'
makeSuitNumTile :: TSuit -> TNumber -> Tile
makeSuitNumTile Manzu n = Man n
makeSuitNumTile Souzu n = Sou n
makeSuitNumTile Pinzu n = Pin n
makeSuitNumTile _     _ = E     -- failure

-- | Find all tiles matching a 'TNumber'
getNumTiles :: Maybe TNumber -> [Tile]
getNumTiles n = filter ((== n) . getTileNum) everyTile


-- | All defined 'Tile's
everyTile :: [Tile]
everyTile = manTiles ++ souTiles ++ pinTiles ++ honorTiles
  where nums = [minBound..maxBound]
        manTiles = map Man nums
        souTiles = map Sou nums
        pinTiles = map Pin nums
        honorTiles = [DGreen,DRed,DWhite,E,S,W,N]

-- | 'Traversal' for the 'TNumber' of a 'Tile' (focus on 0 or 1)
tileNum :: Traversal Tile Tile TNumber TNumber
tileNum f (Man n) = Man <$> f n
tileNum f (Sou n) = Sou <$> f n
tileNum f (Pin n) = Pin <$> f n
tileNum f t       = pure t

-- | 'Getter' for augmented 'TNumber' (returns Nothing fo honors)
maybeTileNum :: Getter Tile (Maybe TNumber)
maybeTileNum f = \t -> coerce $ f $ getTileNum t

-- | 'Getter' for the 'TSuit' of a 'Tile'
tileSuit :: Getter Tile TSuit
tileSuit f = \t -> coerce $ f $ getTileSuit t

-- | 'Fold' for the tiles with a 'TSuit'
suitTiles :: Fold TSuit Tile
suitTiles f = \s -> coerce $ traverse f (getSuitTiles s)
