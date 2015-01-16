{-|
Module      : Parse.MultiSet
Description : Parser combinators for unordered groups ('MultiSet's)
Copyright   : (c) Leon Medvinsky, 2014

License     : GPL-3
Maintainer  : lmedvinsky@hotmail.com
Stability   : experimental
Portability : portable
-}

module Parse.MultiSet (
                        -- | A parser for unordered groups, using 'MultiSet'
                        MultiParser
                      , runParser
                        -- * Basic Parsers
                      , satisfy
                      , matchElem
                      , oneOf
                      , noneOf
                      , nOf
                      , count
                      , forValues
                      , forFilteredValues
                      , getEachOf
                      , catMaybeParse
) where

import           Control.Applicative (Alternative, Applicative, empty, pure,
                                      (<$>), (<*>), (<|>))
import           Control.Arrow       (first, second)
import qualified Data.Foldable       as F
import           Data.Maybe          (mapMaybe, catMaybes)
import           Data.Monoid
import qualified Data.MultiSet       as MS


data MultiParser a b =
    MultiParser { _runParser :: MS.MultiSet a -> [(b, MS.MultiSet a)] }

-- | Takes a 'MultiParser' and a 'MultiSet', generating all possible parses
runParser :: MultiParser a b -> MS.MultiSet a -> [(b, MS.MultiSet a)]
runParser = _runParser

instance Functor (MultiParser a) where
    fmap f = MultiParser . fmap (fmap (first f)) . runParser

instance Applicative (MultiParser a) where
    pure x = MultiParser $ \ms -> pure (x, ms)
    MultiParser f <*> MultiParser g =
        MultiParser $ \ms -> f ms >>= \(resf, rest) ->
                             g rest >>= \(res1, rest2) ->
                             pure (resf res1, rest2)

instance Alternative (MultiParser a) where
    empty = MultiParser $ const []
    MultiParser f <|> MultiParser g =
        MultiParser $ \ms -> f ms ++ g ms

instance Monad (MultiParser a) where
  return = pure
  MultiParser f >>= k = MultiParser $ \a ->
                        let parses = f a
                        in parses >>= \(b, ms) -> runParser (k b) ms

instance Monoid (MultiParser a b) where
  mempty = empty
  mappend = (<|>)

-- Helper function for removing an element from a multiset
removePair :: Ord a => MS.MultiSet a -> a -> (a, MS.MultiSet a)
removePair ms x = (x, MS.delete x ms)

-- | Match all parses getting one elements matching this predicate (O(n))
satisfy :: Ord a => (a -> Bool) -> MultiParser a a
satisfy p = MultiParser $
            \ms -> map (removePair ms) . filter p . MS.elems $ ms

-- | Match an exact element (O(log n))
matchElem :: Ord a => a -> MultiParser a a
matchElem x = MultiParser $
              \ms -> if x `MS.member` ms
                     then [(x, MS.delete x ms)]
                     else []

-- | Match one of the elements provided (O(m×log n))
oneOf :: Ord a => [a] -> MultiParser a a
oneOf xs = MultiParser $
           \ms -> map (removePair ms) . filter (`MS.member` ms) $ xs

-- | Match any element not provided (O(m×n))
noneOf :: Ord a => [a] -> MultiParser a a
noneOf xs = MultiParser $
            \ms -> map (removePair ms) . filter (`notElem` xs) $ MS.toList ms

-- | Match n identical elements (O(log n))
nOf :: Ord a => Int -> a -> MultiParser a [a]
nOf n x = MultiParser $ \ms ->
          case MS.occur x ms of
            n'
               | n' >= n   -> [removeNPair ms x]
               | otherwise -> []
    where removeNPair ms x' = (replicate n x', MS.deleteMany x' n ms)

-- | count n p parses n occurences of p
count :: Int -> MultiParser a b -> MultiParser a [b]
count n p
  | n > 0     = (:) <$> p  <*> count (n - 1) p
  | otherwise = pure []

-- | For each unique value, try a parse
forValues :: (a -> MultiParser a b) -> MultiParser a b
forValues f = MultiParser $ \ms ->
  flip runParser ms . F.foldr (<|>) empty . map f . MS.distinctElems $ ms

-- | Like 'forValues', but a predicate to filter out some values
forFilteredValues :: (a -> Maybe (MultiParser a b)) -> MultiParser a b
forFilteredValues p = MultiParser $ \ms ->
  flip runParser ms .
  F.foldr (<|>) empty . mapMaybe p . MS.distinctElems $ ms

-- | Get a parse with the specified parsers
getEachOf :: [(Int, MultiParser a b)] -> MultiParser a [b]
getEachOf [] = pure []
getEachOf xs = F.asum $ map go slices
  where slices = slice xs
        go (x,xs') = (:) <$> x <*> getEachOf xs'

-- | Gets every view of a list where you have an element and the rest
slice :: [(Int, a)] -> [(a,[(Int, a)])]
slice []     = []
slice ((n,x):xs)
  | n > 1 =
      let y'  = (x,(n-1, x):xs)
          ys' = map (second ((n,x):)) $ slice xs
      in y' : ys'
  | otherwise =
      let y'  = (x, xs)
          ys' = map (second ((1,x):)) $ slice xs
      in y' : ys'

catMaybeParse :: MultiParser a (Maybe b) -> MultiParser a b
catMaybeParse (MultiParser f) = MultiParser $ \ms ->
  catMaybes . map expandMaybe $ f ms
  where expandMaybe (Nothing, _) = Nothing
        expandMaybe (Just x , y) = Just (x, y)
