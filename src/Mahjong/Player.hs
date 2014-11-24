module Mahjong.Player (
                        -- * Hands
                        Hand(..)
                      , ValidHand
                      ) where

import Mahjong.Group
import Mahjong.Tile
import Parse.MultiSet
import Control.Lens

-- | Represents a hand of tiles a player might have
data Hand a =
  Hand { _closedTiles :: [Tile]
       , _openGroups  :: [Group]
       } deriving (Show)

makeLenses ''Hand

data HandValidator = HV { runValidator :: Player -> Hand -> Maybe ValidHand }
