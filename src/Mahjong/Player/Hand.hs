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
              Seat
            , Hand(..)
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
            , WinMethod(..)
            , WinTile(..)
              -- * Winning
            , tryAgari
              -- ** Lenses
            , resWait
            , resWinTile
            , resOpens
            , resCloseds
            , resAtama
            , resWin
              -- * Testing
            , testHand
            , testHand2
            ) where

import           Control.Lens
import           Control.Applicative
import qualified Data.IntMap.Strict as IM
import           Data.List          (intercalate)
import           Data.Maybe
import           Data.Monoid
import qualified Data.MultiSet      as MS
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

data WinTile = Tsumo Tile
             | Ron Tile Direction
             deriving (Eq)

instance Show WinTile where
  show (Tsumo t) = show t ++ " tsumo'd"
  show (Ron t d) = show t ++ " from " ++ show d

data Result =
  Result { _resWait    :: Wait
         , _resWinTile :: WinTile
         , _resOpens   :: [(Group, Seat)]
         , _resCloseds :: [Group]
         , _resAtama   :: Maybe Atama -- ^ Might have won by atama
         , _resWin     :: Either Atama Group -- ^ What was won by
         }
  deriving (Eq)

makeLenses ''Result

instance Show Result where
  show res =
    "Closed: "
    ++ show (res ^. resCloseds)
    ++ " "
    ++ maybe "" show (res ^. resAtama)
    ++ " Open: "
    ++ show (res ^. resOpens)
    ++ " Wait: "
    ++ show (res ^. resWait)
    ++ " + "
    ++ show (res ^. resWinTile)

-- | Tests if the hand has the right number of tiles
wellFormed :: Hand -> Bool
wellFormed h = cTNum + oGNum + cGNum == (12 :: Int)
  where cTNum = countOf (closedTiles.folded) h -- Closed tile count

        -- open groups tile count
        oGNum = countOf (openGroups.traversed) h * 3

        -- closed groups tile count
        cGNum = countOf (closedGroups.traversed) h * 3

        -- Gets the count of things in a traversal
        countOf l = getSum . foldMapOf l (const (Sum 1))

-- | Test hand for open groups
testHand :: Hand
testHand =
  Hand { _newTile = Just $ Dragon R
       , _closedTiles = IM.fromList $ [1..] `zip` (replicate 2 (Dragon R)
                                      ++ replicate 3 (Dragon H)
                                      ++ replicate 3 (Wind S)
                                      ++ replicate 2 (Wind N))
       , _openGroups  = [(Kou (Dragon G) (Dragon G) (Dragon G), W)]
       , _closedGroups = []
       }

-- | Test hand for multiple interpretations
testHand2 :: Hand
testHand2 =
  Hand { _newTile = Just $ NumT Sou One
       , _closedTiles = IM.fromList $ [1..] `zip` (replicate 2 (Dragon R)
                                      ++ replicate 2 (NumT Sou One)
                                      ++ replicate 3 (NumT Sou Two)
                                      ++ replicate 3 (NumT Sou Three)
                                      ++ replicate 3 (Dragon G))
       , _openGroups  = []
       , _closedGroups = []
       }

data WinMethod = MTsumo
               | MRon Direction
               deriving (Show, Eq)

-- | Utility function. Not exported.
combineMethodTile :: WinMethod -> Tile -> WinTile
combineMethodTile MTsumo    t = Tsumo t
combineMethodTile (MRon d)  t = Ron t d

-- | Try to create valid 'Result's from this 'Hand'.
tryAgari :: WinMethod -> Hand -> [Result]
tryAgari method hand = fromMaybe [] $ -- Convert the Maybe list of Results to
                                      -- just a list of Results

  map fst . -- We don't want the remaining MultiSet (which should be empty)

  -- Get rid of Results that have not used all the tiles
  (\t -> filter (MS.null . snd) (runParser (allResP t) handSet))

  -- fmap this onto the possible newTile. It will be Nothing if there isn't an
  -- additional tile (and then it should fail).
  -- ???: Should I differentiate between newTiles that come from Ron or Tsumo?
  <$> (hand ^. newTile)
  
  where -- The set of closed tiles converted to MultiSet, for parsing
        handSet = MS.fromList . map snd . IM.toList $ (hand ^.closedTiles)

        -- How many groups we need to make. 4 - groups already declared
        remGroups =
          4 - hand ^.openGroups.to length - hand ^.closedGroups.to length

        -- Parser for either three-tile group
        threeParser = kouParser <|> shunParser

        -- Parser for any of the three shuntsu waits
        threeWParser = ryanParser <|> kanchParser <|> penParser

        -- These parsers parser the remaining closed tiles into intermediate
        -- results, which are different for each "class" of wait (shanpon,
        -- tanki, or any of the two-tile waits)
        shanHandP = (,) <$> count (remGroups - 1) threeParser <*> shanParser
        tankiHandP = (,) <$> count remGroups threeParser <*> tanParser
        normHandP =
          (,,) <$> count (remGroups - 1) threeParser
               <*> atamaParser
               <*> threeWParser

        -- These parsers parse the remaining closed tiles into results
        shanResP new = catMaybeParse $ waitingFinishSh new <$> shanHandP
        tankiResP new = catMaybeParse $ waitingFinishTan new <$> tankiHandP
        normResP new = catMaybeParse $ waitingFinishNorm new <$> normHandP
        allResP new = shanResP new <|> tankiResP new <|> normResP new

        -- These functions take the output of the HandPs and creates Result
        -- values from them (and the tile in-hand)
        waitingFinishSh t (gs, sh) =
          case shanponCheck t sh of
           Just (ata, g) -> Just
             Result { _resWait    = sh
                    , _resWinTile = combineMethodTile method t
                    , _resOpens   = hand ^.openGroups
                    , _resCloseds = hand ^.closedGroups ++ gs
                    , _resAtama   = Just ata
                    , _resWin     = Right g
                    } 
           Nothing -> Nothing
        waitingFinishTan t (gs, tank) =
          case tankiCheck t tank of
           Just ata -> Just
             Result { _resWait    = tank
                    , _resWinTile = combineMethodTile method t
                    , _resOpens   = hand ^.openGroups
                    , _resCloseds = hand ^.closedGroups ++ gs
                    , _resAtama   = Nothing
                    , _resWin     = Left ata
                    } 
           Nothing  -> Nothing
        waitingFinishNorm t (gs, ata, w) =
          case waitToGroup t w of
           Just g  -> Just
             Result { _resWait    = w
                    , _resWinTile = combineMethodTile method t
                    , _resOpens   = hand ^.openGroups
                    , _resCloseds = hand ^.closedGroups ++ gs
                    , _resAtama   = Just ata
                    , _resWin     = Right g
                    }
           Nothing -> Nothing
