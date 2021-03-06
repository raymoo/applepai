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
            , handWaits
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
              -- ** Query
            , winTileMethod
              -- ** Winning
            , tryAgari
              -- ** Lenses
            , resWait
            , resWinTile
            , resOpens
            , resCloseds
            , resMAtama
            , resWin
            , resAtama
            , resGroups
            , resAllClosedGroups
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

winTileTile :: Lens' WinTile Tile
winTileTile f (Tsumo t) = Tsumo <$> f t
winTileTile f (Ron t d) = (\t' -> Ron t' d) <$> f t


instance Show WinTile where
  show (Tsumo t) = show t ++ " tsumo'd"
  show (Ron t d) = show t ++ " from " ++ show d

data Result =
  Result { _resWait    :: Wait
         , _resWinTile :: WinTile
         , _resOpens   :: [(Group, Seat)]
         , _resCloseds :: [Group]
         , _resMAtama  :: Maybe Atama -- ^ Might have won by atama
         , _resWin     :: Either Atama Group -- ^ What was won by
         }
  deriving (Eq)

makeLenses ''Result

resAtama :: Lens' Result Atama
resAtama f res =
  case res^.resMAtama of
   Just ata -> (\a -> res & resMAtama .~ Just a) <$> f ata
   -- Dangerous, but res should have atama in win if it went here
   Nothing ->
     (\a -> res & resWin .~ Left a) <$> f (either id (error "Shouldn't happen (resAtama)") (res^.resWin))

resGroups :: Fold Result Group
resGroups f = \res ->
  let groups = oGroups ++ cGroups ++ wGroup
      oGroups = map fst $ res^.resOpens
      cGroups = res^.resCloseds
      wGroup = res^..resWin._Right
  in coerce (traverse f groups)

resAllClosedGroups :: Fold Result Group
resAllClosedGroups f = \res ->
  let cGroups = res^.resCloseds
      wGroup = case res^.resWinTile.to winTileMethod of
                MTsumo -> res^..resWin._Right
                MRon _ _ -> []
  in coerce $ traverse f (wGroup ++ cGroups)

instance Show Result where
  show res =
    "Closed: "
    ++ show (res ^. resCloseds)
    ++ " "
    ++ maybe "" show (res ^. resMAtama)
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
               | MRon Tile Direction
               deriving (Show, Eq)



winTileMethod :: WinTile -> WinMethod
winTileMethod (Tsumo _) = MTsumo
winTileMethod (Ron t d) = MRon t d


-- | Utility function. Not exported.
foldWinMethod :: b -> (Tile -> Direction -> b) -> WinMethod -> b
foldWinMethod tsumo _    MTsumo     = tsumo
foldWinMethod _     ronf (MRon t d) = ronf t d

-- These parsers parse the remaining closed tiles into intermediate
-- results, which are different for each "class" of wait (shanpon,
-- tanki, or any of the two-tile waits). The [Group] are all the closed
-- groups.
--
-- The Int argument is how many groups are needed.

-- | The 'Wait' is always 'Shanpon'
shanHandP :: Int -> MultiParser Tile ([Group], Wait)
shanHandP n = (,) <$> count (n - 1) threeParser <*> shanParser

-- | The 'Wait' is always 'Tanki'
tankiHandP :: Int -> MultiParser Tile ([Group], Wait)
tankiHandP n = (,) <$> count n threeParser <*> tanParser

-- | The 'Wait' is Kanchan, Penchan, or Ryanmen. This one has Atama separate
-- since it is not involved in the 'Wait', unlike shanpon or tanki.
normHandP :: Int -> MultiParser Tile ([Group], Atama, Wait)
normHandP n =
  (,,) <$> count (n - 1) threeParser
  <*> atamaParser
  <*> threeWParser

-- | Parser for either three-tile group
threeParser :: MultiParser Tile Group
threeParser = kouParser <|> shunParser


-- | Parser for any of the three shuntsu waits
threeWParser :: MultiParser Tile Wait
threeWParser = ryanParser <|> kanchParser <|> penParser

-- | Get the waits for a hand, ignoring any new tile that may have been drawn
handWaits :: Hand -> [Wait]
handWaits hand =
  let results = runParser (shanParse <|> tanParse <|> normParse) remTiles
  in map fst . filter (MS.null . snd) $ results
  where needed = 4 - hand^.openGroups.to length
        shanParse = snd <$> shanHandP needed
        tanParse = snd <$> tankiHandP needed
        normParse = view _3 <$> normHandP needed
        remTiles = MS.fromList $ hand^..closedTiles.traverse

-- | Try to create valid 'Result's from this 'Hand'.
tryAgari :: WinMethod -> Hand -> [Result]
tryAgari method hand = fromMaybe [] $ -- Convert the Maybe list of Results to
                                      -- just a list of Results

  map fst . -- We don't want the remaining MultiSet (which should be empty)

  -- Get rid of Results that have not used all the tiles
  (\winT -> filter (MS.null . snd) (runParser (allResP winT) handSet))

  -- fmap this onto the possible newTile. It will be Nothing if there isn't an
  -- additional tile (and then it should fail).
  <$> agariWinTile
  
  where -- What was used to win
        agariWinTile =
          foldWinMethod (fmap Tsumo $ hand^.newTile) ((Just .) . Ron) method

        -- The set of closed tiles converted to MultiSet, for parsing
        handSet = MS.fromList . map snd . IM.toList $ (hand ^.closedTiles)

        -- How many groups we need to make. 4 - groups already declared
        remGroups =
          4 - hand ^.openGroups.to length - hand ^.closedGroups.to length

        -- These parsers parse the remaining closed tiles into results
        shanResP new = catMaybeParse $ waitingFinishSh new <$> shanHandP remGroups
        tankiResP new = catMaybeParse $ waitingFinishTan new <$> tankiHandP remGroups
        normResP new = catMaybeParse $ waitingFinishNorm new <$> normHandP remGroups
        allResP winTile = shanResP winTile
                      <|> tankiResP winTile
                      <|> normResP winTile

        -- These functions take the output of the HandPs and creates Result
        -- values from them (and the tile in-hand)
        waitingFinishSh wintile (groups, wait) =
          case shanponCheck (wintile^.winTileTile) wait of
           Just (ata, group) -> Just
             Result { _resWait    = wait
                    , _resWinTile = wintile
                    , _resOpens   = hand ^.openGroups
                    , _resCloseds = hand ^.closedGroups ++ groups
                    , _resMAtama  = Just ata
                    , _resWin     = Right group
                    } 
           Nothing -> Nothing
        waitingFinishTan wintile (groups, wait) =
          case tankiCheck (wintile^.winTileTile) wait of
           Just ata -> Just
             Result { _resWait    = wait
                    , _resWinTile = wintile
                    , _resOpens   = hand ^.openGroups
                    , _resCloseds = hand ^.closedGroups ++ groups
                    , _resMAtama  = Nothing
                    , _resWin     = Left ata
                    } 
           Nothing  -> Nothing
        waitingFinishNorm wintile (groups, ata, wait) =
          case waitToGroup (wintile^.winTileTile) wait of
           Just group  -> Just
             Result { _resWait    = wait
                    , _resWinTile = wintile
                    , _resOpens   = hand ^.openGroups
                    , _resCloseds = hand ^.closedGroups ++ groups
                    , _resMAtama  = Just ata
                    , _resWin     = Right group
                    }
           Nothing -> Nothing
