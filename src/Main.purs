module Main where

import Data.Unfoldable (unfoldr)
import Data.Generic (class Generic, gShow, gEq)
import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Map as Map
import Data.Map (Map)
import Data.Array as Array
import Data.List as List
import Data.List (List(..),(:))
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.String (toCharArray)
import Data.Tuple (Tuple(Tuple))

frequency :: String -> Map Char Int
frequency str =
  Array.foldl (flip (Map.alter updater))
      Map.empty
      (toCharArray str)
    where updater = Maybe.maybe 1 ((+) 1) >>> Just

toTree :: Map Char Int -> List Tree
toTree m =
    (\(Tuple k v) -> Leaf k v) <$> list
    where list =  Map.toUnfoldable m

data Tree
  = Leaf Char Int
  | Branch Tree Tree

derive instance genericTree :: Generic Tree

instance showTree :: Show Tree where
  show (Leaf k v) = show k <> " " <> show v
  show (Branch l r) = show l <> "\n" <> show r

instance eqTree :: Eq Tree where
  eq = gEq

weight :: Tree -> Int
weight (Leaf k v) = v
weight (Branch left right) = (weight left) + (weight right)

instance ordTree :: Ord Tree where
  compare a b = compare (weight a) (weight b)

huffman :: List Tree -> Maybe Tree
huffman xs =
  step $ List.sort xs
  where step Nil = Nothing
        step (Cons x Nil) = Just x
        step (Cons x (Cons y ys)) = huffman (Branch x y : ys)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
  log $ show probabilities
  log $ show $ huffman probabilities
  where phrase = "Donald Trump is just the best coder. Really great."
        probabilities = toTree $ frequency phrase
        keyOf v = v.key
        minKey m = keyOf <$> Map.findMin m
