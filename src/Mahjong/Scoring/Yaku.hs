{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
module Mahjong.Scoring.Yaku (
                              -- * Yaku
                              YakuRule
                            , YakuResult(..)
                            , YakuContext(..)
                            , yaku
                            , mutEx
                            , combine
                            , simpleYaku
                             -- ** Lenses
                            , ycPlayerWind
                            , ycRiichi
                            , ycTableWind
                            , ycRes
                            , yrYakuman
                            , yrTotal
                            , yrPairs
                              -- ** Finding
                            , findYaku
                              -- ** Examples
                            , dragon
                            , jifuu
                            , bafuu
                            , yakuhai
                            , sanankou
                            , toitoi
                            , honrou
                            , chinrou
                            , pinfu
                            , iipei
                            , ryanpei
                            , sanshokudoujun
                            , ittsuu
                            , stdYaku
                            ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Control.Lens
import Data.Function
import Data.List (maximumBy, foldl', group, nub)
import Data.Functor.Foldable hiding (Foldable)

import Mahjong.Player.Hand
import Mahjong.Group
import Mahjong.Tile
import Mahjong.Group.Wait


-- | The data yaku operate on
data YakuContext = YakuContext { _ycPlayerWind :: Direction
                               , _ycTableWind :: Direction
                               , _ycRiichi :: Bool
                               , _ycRes :: Result
                               }
                 deriving (Show, Eq)

makeLenses ''YakuContext


-- | A rule for determining Han. Semantically, is a function from 'Result's to
-- a list of pairs of yaku names and han values, in addition to a total value
data RulePiece a = Yaku (YakuContext -> Maybe (String, YakuValue))
                 | MutEx [a]
                 | Combine [a]
                 deriving (Functor)


type YakuRule = Fix RulePiece


-- | A normal yaku, taking in information and spitting out scoring data
yaku :: (YakuContext -> Maybe (String, YakuValue)) -> YakuRule
yaku = Fix . Yaku


-- | Mutually exclusive rules, selecting the highest score, left biased.
mutEx :: [YakuRule] -> YakuRule
mutEx = Fix . MutEx


combine :: [YakuRule] -> YakuRule
combine = Fix . Combine


-- | The value of a yaku. 'YakuNorm' is for normal yaku with a han value, while
-- 'Yakuman' is for yakuman hands. It is parametrized by an 'Int' just in case
-- you have double yakuman hands or similar things.
data YakuValue = YakuNorm Int
               | Yakuman  Int
               deriving (Show, Eq, Ord)


-- | The result of searching for yaku.
data YakuResult = YakuResult { _yrYakuman :: Int
                             , _yrTotal :: Int
                             , _yrPairs :: [(String, YakuValue)]
                             }
                deriving (Show, Eq)

makeLenses ''YakuResult


-- | Helper function that merges two 'YakuResult's
addResults :: YakuResult -> YakuResult -> YakuResult
addResults (YakuResult ym1 t1 ps1) (YakuResult ym2 t2 ps2) =
  YakuResult (ym1 + ym2) (t1 + t2) (ps1 ++ ps2)


-- | Result where no yaku are found
emptyRes :: YakuResult
emptyRes = YakuResult 0 0 []


findPiece :: YakuContext -> RulePiece YakuResult -> YakuResult
findPiece yc (Yaku f) = case f yc of
                         Nothing -> emptyRes
                         Just res@(_, YakuNorm han) -> YakuResult 0 han [res]
                         Just res@(_, Yakuman ym)   -> YakuResult ym 0 [res]
findPiece _ (MutEx yrs) =
  let makeComp yr = [yr^.yrYakuman, yr^.yrTotal]
  in maximumBy (compare `on` makeComp) (emptyRes : yrs)
findPiece _ (Combine yrs) = foldl' addResults emptyRes yrs
                         


-- | Determines the best-scoring combination of yaku using a particular scoring
-- scheme and the required 'YakuContext'
findYaku :: YakuRule -> YakuContext -> YakuResult
findYaku yr yc = cata (findPiece yc) yr
  

-- | Helper function for determining if a hand is closed
isClosed :: Result -> Bool
isClosed res = length (res^..resGroups) == length (res^..resAllClosedGroups)


-- | Similar, for openness
isOpen :: Result -> Bool
isOpen = not . isClosed


-- | Is it a group of the same kind of tile?
isSames :: Group -> Bool
isSames = (||) <$> isGroupType Kantsu <*> isGroupType Koutsu


-- | If a rule only needs to check the 'Result', use this combinator
simpleYaku :: (Result -> Maybe (String, YakuValue)) -> YakuRule
simpleYaku f = yaku $ \yc -> f $ yc^.ycRes


-- | Toitoi rule
toitoi :: YakuRule
toitoi = simpleYaku go
  where go res
          | and (res^..resGroups.to isSames) = Just ("Toi Toi", YakuNorm 2)
          | otherwise                        = Nothing
  

containsT :: Tile -> Group -> Bool
containsT t g = t `elem` g^..groupTiles


-- | Helper function for creating dragon yakuhai yaku
colorDragon :: String -> DColor -> YakuRule
colorDragon name col = simpleYaku $ \res ->
  let dCount = counter (res^..resGroups)
  in if dCount == 0 then Nothing else Just (name, YakuNorm dCount)
  where contD = containsT (Dragon col)
        counter = length . filter contD

greenDragon :: YakuRule
greenDragon = colorDragon "Ryoku" G

redDragon :: YakuRule
redDragon = colorDragon "Chun" R

whiteDragon :: YakuRule
whiteDragon = colorDragon "Haku" H


-- | Covers the different dragon yakuhai
dragon :: YakuRule
dragon = combine [greenDragon, redDragon, whiteDragon]


-- | Self Wind
jifuu :: YakuRule
jifuu = yaku go
  where go (YakuContext pd _ _ res)
          | numGs > 0 = Just ("Self Wind", YakuNorm numGs)
          | otherwise = Nothing
          where contW = containsT (Wind pd)
                -- Should be 1 at max - only four of each wind in play
                numGs = length . filter contW $ res^..resGroups


-- | Table Wind
bafuu :: YakuRule
bafuu = yaku go
  where go (YakuContext _ td _ res)
          | numGs > 0 = Just ("Self Wind", YakuNorm numGs)
          | otherwise = Nothing
          where contW = containsT (Wind td)
                -- Should be 1 at max - only four of each wind in play
                numGs = length . filter contW $ res^..resGroups

-- | All yakuhai
yakuhai :: YakuRule
yakuhai = combine [dragon, jifuu, bafuu]


-- | Helper function for seeing if a group is some type
isGroupType :: GroupType -> Group -> Bool
isGroupType gt g = groupType g == gt


-- | Helper function to create ankou groups.
nAnkou :: String -> YakuValue -> Int -> YakuRule
nAnkou name yv n = simpleYaku $ \res ->
  let count = numKouKan (res^..resAllClosedGroups)
  in if count >= n then Just (name, yv) else Nothing
  where numKouKan gs = length (filter isSames gs)


-- | Sanankou rule
sanankou :: YakuRule
sanankou = nAnkou "San An Kou" (YakuNorm 2) 3


honrou :: YakuRule
honrou = simpleYaku go
  where go res
          | and (res^..resGroups.groupTiles.to valid) &&
            and (res^..resGroups.to containsEdge) = Just ("Hon Rou Tou", YakuNorm 2)
          | otherwise = Nothing
        valid = (||) <$> isEdge <*> isHonor
        containsEdge g = or $ g^..groupTiles.to isEdge

chinrou :: YakuRule
chinrou = simpleYaku go
  where go res
          | and (res^..resGroups.groupTiles.to isEdge) = Just ("Chin Rou Tou", Yakuman 1)
          | otherwise = Nothing


-- | Chinroutou and Honroutou rule
routou :: YakuRule
routou = mutEx [honrou, chinrou]


-- | Suuankou rule
suuankou :: YakuRule
suuankou = nAnkou "Suu An Kou" (Yakuman 1) 4


-- | Pinfu rule
pinfu :: YakuRule
pinfu = yaku go
  where go yc
          | yc^.ycRes.resWait.to isRyanmen &&
            and (yc^..ycRes.resGroups.to isShuntsu) &&
            isClosed (yc^.ycRes) &&
            none (`elem` [yc^.ycPlayerWind, yc^.ycTableWind])
                 (yc^..ycRes.resAtama.traverse.atamaTiles.to tileWind.traverse) =
              Just ("Pinfu", YakuNorm 1)
          | otherwise = Nothing
        isShuntsu Shun{} = True
        isShuntsu _      = False
        isRyanmen Ryanmen{} = True
        isRyanmen _         = False


-- | Helper function for iipei and ryanpei
nPei :: String -> Int -> YakuRule
nPei name n = simpleYaku go
  where go res
          | isOpen res = Nothing
          | nDup (res^..resGroups.filtered (\g -> groupType g == Shuntsu)) =
              Just (name, YakuNorm 1)
          | otherwise = Nothing
        duplicates xs = filter (> 1) . map length . group $ xs
        nDup xs = length (duplicates xs) == n

-- | iipeikou
iipei :: YakuRule
iipei = nPei "Ii Pei Kou" 1


-- | ryanpeikou
ryanpei :: YakuRule
ryanpei = nPei "Ryan Pei Kou" 2


-- | Covers iipeikou and ryanpeikou
pei :: YakuRule
pei = mutEx [iipei, ryanpei]


-- | Helper function, gets view of a list where one element is cut out. Order
-- is not defined.
splices :: [a] -> [(a,[a])]
splices = go []
  where go _  [] = []
        go xs (y:ys) = (y, xs ++ ys) : go (y:xs) ys


-- | Gets different choices of numbers of elements from a list. Order is not
-- defined.
chooses :: Int -> [a] -> [[a]]
chooses 0 _  = [[]]
chooses n xs
  | n < 0 = []
  | otherwise = do
      (one, rest) <- splices xs
      otherPart <- chooses (n - 1) rest
      return $ one : otherPart


-- | Inefficient, but sufficient for small inputs
uniq :: Eq a => [a] -> Bool
uniq xs = length xs == length (nub xs)


-- | Basically opposite of uniq
ident :: Eq a => [a] -> Bool
ident [] = True
ident xs = length (nub xs) == 1


-- | Sanshoku Doujun rule
sanshokudoujun :: YakuRule
sanshokudoujun = simpleYaku $ go
  where diffColors gs = uniq . map (getTileSuit . head . toListOf groupTiles) $ gs
        sameNums gs = ident $ map (toListOf $ groupTiles.to getTileNum.traverse) gs
        go res
          | not . null $ groupOfGroups = Just ("San Shoku Dou Jun", YakuNorm score)
          | otherwise = Nothing
          where shunGroups = filter (isGroupType Shuntsu) $ res^..resGroups
                score
                  | isOpen res = 1
                  | otherwise = 2
                groupOfGroups = do
                  choice <- chooses 3 shunGroups
                  guard $ diffColors choice
                  guard $ sameNums choice
                  return choice


-- | Ikki Tsuukan
ittsuu :: YakuRule
ittsuu = simpleYaku $ go
  where go res
          | null goodGroups = Nothing
          | otherwise = Just ("Ikki Tsuu Kan", score)
          where shunGroups = filter (isGroupType Shuntsu) $ res^..resGroups
                sameColor gs = ident . map (getTileSuit . head . toListOf groupTiles) $ gs
                onetwothree    = [One .. Three]
                fourfivesix    = [Four .. Six]
                seveneightnine = [Seven .. Nine]
                numSeqs = map (toListOf $ groupTiles.to getTileNum.traverse)
                isStraight gs = let seqs = numSeqs gs
                                in onetwothree `elem` seqs &&
                                   fourfivesix `elem` seqs &&
                                   seveneightnine `elem` seqs
                score
                  | isOpen res = YakuNorm 1
                  | otherwise = YakuNorm 2
                goodGroups = do
                  choice <- chooses 3 shunGroups
                  guard (sameColor choice)
                  guard (isStraight choice)
                  return choice


-- | Doesn't actually contain all standard yaku yet. Should eventually.
stdYaku :: YakuRule
stdYaku = combine [ toitoi
                  , yakuhai
                  , sanankou
                  , routou
                  , suuankou
                  , pinfu
                  , pei
                  , sanshokudoujun
                  , ittsuu
                  ]


-- | Test result for pinfu and sanshoku
testRes :: Result
testRes = Result { _resWait = Ryanmen (NumT Sou Two) (NumT Sou Three)
                 , _resWinTile = Tsumo (NumT Sou Four)
                 , _resOpens = []
                 , _resCloseds = [ Shun (NumT Man Two) (NumT Man Three) (NumT Man Four)
                                 , Shun (NumT Pin Two) (NumT Pin Three) (NumT Pin Four)
                                 , Shun (NumT Sou Six) (NumT Sou Seven) (NumT Sou Eight)
                                 ]
                 , _resAtama = Just (Atama (Wind S) (Wind S))
                 , _resWin = Right (Shun (NumT Sou Two) (NumT Sou Three) (NumT Sou Four))
                 } 


-- | Test context for the same as above
testContext :: YakuContext
testContext = YakuContext W E True testRes
