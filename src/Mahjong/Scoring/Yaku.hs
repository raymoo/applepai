{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
module Mahjong.Scoring.Yaku (
                              -- * Yaku
                              YakuRule(..)
                            , YakuResult(..)
                            , YakuContext(..)
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
                            , sanankou
                            , toitoi
                            , honrou
                            , chinrou
                            , pinfu
                            , iipei
                            , ryanpei
                            , stdYaku
                            ) where

import Control.Applicative ((<$>), (<*>))
import Control.Lens
import Data.Function
import Data.List (maximumBy, foldl', group)

import Mahjong.Player.Hand
import Mahjong.Group
import Mahjong.Tile
import Mahjong.Group.Wait


data YakuContext = YakuContext { _ycPlayerWind :: Direction
                               , _ycTableWind :: Direction
                               , _ycRiichi :: Bool
                               , _ycRes :: Result
                               }
                 deriving (Show, Eq)

makeLenses ''YakuContext


-- | A rule for determining Han. Semantically, is a function from 'Result's to
-- a list of pairs of yaku names and han values, in addition to a total value
data YakuRule = Yaku (YakuContext -> Maybe (String, YakuValue))
              | MutEx [YakuRule]   -- ^ Mutually exclusive rules.
                                   -- The result should be the first of the maximal
                                   -- values.
              | Combine [YakuRule] -- ^ Every rule applies, so all the results come
                                   -- together.


data YakuValue = YakuNorm Int
               | Yakuman  Int
               deriving (Show, Eq, Ord)


data YakuResult = YakuResult { _yrYakuman :: Int
                             , _yrTotal :: Int
                             , _yrPairs :: [(String, YakuValue)]
                             }
                deriving (Show, Eq)

makeLenses ''YakuResult


addResults :: YakuResult -> YakuResult -> YakuResult
addResults (YakuResult ym1 t1 ps1) (YakuResult ym2 t2 ps2) =
  YakuResult (ym1 + ym2) (t1 + t2) (ps1 ++ ps2)

emptyRes :: YakuResult
emptyRes = YakuResult 0 0 []


findYaku :: YakuRule -> YakuContext -> YakuResult
findYaku (Yaku f) yc =
  case f yc of
   Nothing -> YakuResult 0 0 []
   Just res@(_, YakuNorm han)  -> YakuResult 0 han [res]
   Just res@(_, Yakuman  ym) -> YakuResult ym 0 [res]
findYaku (MutEx ys) yc =
  let makeComp yr = [yr^.yrYakuman, yr^.yrTotal]
  in maximumBy (compare `on` makeComp) . map (flip findYaku yc) $ ys
findYaku (Combine ys) yc = foldl' addResults emptyRes . map (flip findYaku yc) $ ys
  

isClosed :: Result -> Bool
isClosed res = length (res^..resGroups) == length (res^..resAllClosedGroups)


isOpen :: Result -> Bool
isOpen = not . isClosed


isSames :: Group -> Bool
isSames = (||) <$> isGroupType Kantsu <*> isGroupType Koutsu


simpleYaku :: (Result -> Maybe (String, YakuValue)) -> YakuRule
simpleYaku f = Yaku $ \yc -> f $ yc^.ycRes

-- | Toitoi rule
toitoi :: YakuRule
toitoi = simpleYaku go
  where go res
          | and (res^..resGroups.to isSames) = Just ("Toi Toi", YakuNorm 2)
          | otherwise                        = Nothing
  


colorDragon :: String -> DColor -> YakuRule
colorDragon name col = simpleYaku $ \res ->
  let dCount = counter (res^..resGroups)
  in if dCount == 0 then Nothing else Just (name, YakuNorm dCount)
  where contD = (> 0) . length . (filter (== Dragon col)) . getGroupTiles
        counter = length . filter contD

greenDragon :: YakuRule
greenDragon = colorDragon "Ryoku" G

redDragon :: YakuRule
redDragon = colorDragon "Chun" R

whiteDragon :: YakuRule
whiteDragon = colorDragon "Haku" H

dragon :: YakuRule
dragon = Combine [greenDragon, redDragon, whiteDragon]


isGroupType :: GroupType -> Group -> Bool
isGroupType gt g = groupType g == gt


nAnkou :: String -> YakuValue -> Int -> YakuRule
nAnkou name yv n = simpleYaku $ \res ->
  let count = numKouKan (res^..resAllClosedGroups)
  in if count >= n then Just (name, yv) else Nothing
  where numKouKan gs = length (filter isSames gs)

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

routou :: YakuRule
routou = MutEx [honrou, chinrou]


suuankou :: YakuRule
suuankou = nAnkou "Suu An Kou" (Yakuman 1) 4


pinfu :: YakuRule
pinfu = Yaku go
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


nPei :: Int -> YakuRule
nPei n = simpleYaku go
  where go res
          | isOpen res = Nothing
          | nDup (res^..resGroups.filtered (\g -> groupType g == Shuntsu)) =
              Just ("Ii Pei Kou", YakuNorm 1)
          | otherwise = Nothing
        duplicates xs = filter (> 1) . map length . group $ xs
        nDup xs = length (duplicates xs) == n

iipei :: YakuRule
iipei = nPei 1

ryanpei :: YakuRule
ryanpei = nPei 2

pei :: YakuRule
pei = MutEx [iipei, ryanpei]


-- | Doesn't actually contain all standard yaku yet
stdYaku :: YakuRule
stdYaku = Combine [ toitoi
                  , dragon
                  , sanankou
                  , routou
                  , suuankou
                  , pinfu
                  , pei
                  ]
