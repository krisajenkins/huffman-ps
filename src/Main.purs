module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array as Array
import Data.Generic (class Generic, gEq)
import Data.List (List(..),(:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..),maybe)
import Data.String (fromCharArray, toCharArray)
import Data.Tuple (Tuple(Tuple))
import Data.Unfoldable (class Unfoldable)
import Prelude

frequency :: String -> Map Char Int
frequency str =
  Array.foldl (flip (Map.alter updater))
      Map.empty
      (toCharArray str)
    where updater = Just <<< maybe 1 ((+) 1)

toTree :: forall f. (Functor f, Unfoldable f) => Map Char Int -> f Tree
toTree map =
    toLeaf <$> Map.toUnfoldable map

toLeaf :: Tuple Char Int -> Tree
toLeaf (Tuple k v) = Leaf k v

------------------------------------------------------------

data Tree
  = Leaf Char Int
  | Branch Tree Tree

derive instance genericTree :: Generic Tree

instance eqTree :: Eq Tree where
  eq = gEq

instance ordTree :: Ord Tree where
  compare a b = compare (weight a) (weight b)

instance showTree :: Show Tree where
  show = pretty ""

pretty :: String -> Tree -> String
pretty prefix (Leaf k v) = prefix <> show v <> ": " <> show k
pretty prefix branch@(Branch l r) =
  pretty newprefix l <> "\n" <> pretty whiteprefix r
  where newprefix = prefix <> show (weight branch) <> "--"
        whiteprefix = fromCharArray $ const ' ' <$> toCharArray newprefix

weight :: Tree -> Int
weight (Leaf k v) = v
weight (Branch left right) = weight left + weight right

------------------------------------------------------------

huffman :: List Tree -> Maybe Tree
huffman = step <<< List.sort
  where
    step Nil = Nothing
    step (Cons x Nil) = Just x
    step (Cons x (Cons y ys)) =
      huffman $ if weight x < weight y
                then (Branch x y : ys)
                else (Branch y x : ys)

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = do
  log phrase
  log $ maybe "Empty" show encoded
  where phrase = "Donald Trump is just the best coder. Really great."
        probabilities = toTree $ frequency phrase
        encoded = huffman probabilities
