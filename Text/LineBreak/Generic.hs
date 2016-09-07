{-# LANGUAGE FlexibleInstances #-}
module Text.LineBreak.Generic where

import           Data.Text ( Text )
import qualified Data.Text as T

class StringLike a where
  snoc      :: a -> Char -> a
  empty     :: a
  uncons    :: a -> Maybe (Char, a)
  append    :: a -> a -> a
  concat    :: [a] -> a
  singleton :: Char -> a
  span      :: (Char -> Bool) -> a -> (a, a)
  pack      :: [Char] -> a
  unpack    :: a -> [Char]
  length    :: a -> Int
  tail      :: a -> a
  null      :: a -> Bool
  concatMap :: (Char -> a) -> a -> a
  takeWhile :: (Char -> Bool) -> a -> a
  lines     :: a -> [a]

instance StringLike [Char] where
  snoc xs x     = xs ++ [x]
  empty         = []
  uncons []     = Nothing
  uncons (x:xs) = Just (x,xs)
  append        = (++)
  concat        = Prelude.concat
  singleton x   = [x]
  span          = Prelude.span
  pack          = id
  unpack        = id
  length        = Prelude.length
  tail          = Prelude.tail
  null          = Prelude.null
  concatMap     = Prelude.concatMap
  takeWhile     = Prelude.takeWhile
  lines         = Prelude.lines

instance StringLike Text where
  snoc      = T.snoc
  empty     = T.empty
  uncons    = T.uncons
  append    = T.append
  concat    = T.concat
  singleton = T.singleton
  span      = T.span
  pack      = T.pack
  unpack    = T.unpack
  length    = T.length
  tail      = T.tail
  null      = T.null
  concatMap = T.concatMap
  takeWhile = T.takeWhile
  lines     = T.lines
