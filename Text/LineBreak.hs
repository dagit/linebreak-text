{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Text.LineBreak
-- Copyright   :  (C) 2014 Francesco Ariis
-- License     :  BSD3 (see LICENSE file)
--
-- Maintainer  :  Francesco Ariis <fa-ml@ariis.it>
-- Stability   :  provisional
-- Portability :  portable
--
-- Simple functions to break a Text to fit a maximum text width, using
-- Knuth-Liang hyphenation algorithm.
--
-- Example:
--
-- > import Text.Hyphenation
-- > import Text.LineBreak
-- >
-- > hyp = Just english_US
-- > bf = BreakFormat 25 4 '-' hyp
-- > cs = "Using hyphenation with gruesomely non parsimonious wording."
-- > main = putStr $ breakText bf cs
--
-- will output:
--
-- > Using hyphenation with
-- > gruesomely non parsimo-
-- > nious wording.
--
-------------------------------------------------------------------------------

module Text.LineBreak ( breakText, breakTextLn, BreakFormat(..) ) where

import           Prelude hiding (span, lines, null, length, concatMap, takeWhile, concat, tail)
import           Text.LineBreak.Generic
import           Text.Hyphenation
import           Data.Char ( isSpace )
import           Data.List ( inits, tails, find )
import           Data.Monoid ( (<>) )

-- TODO: tabs are broken (as it is just a plain substitution). Use a
--       smart sub method. [bug] [test]


-----------
-- TYPES --
-----------

-- | How to break the texts: maximum width of the lines, number of spaces
-- to replace tabs with (dumb replacement), symbol to use to hyphenate
-- words, hypenator to use (language, exceptions, etc.; refer to
-- "Text.Hyphenation" for usage instructions). To break lines without
-- hyphenating, put @Nothing@ in @bfHyphenator@.
data BreakFormat = BreakFormat { bfMaxCol :: Int,
                                 bfTabRep :: Int,
                                 bfHyphenSymbol :: Char,
                                 bfHyphenator :: Maybe Hyphenator }

data BrState a = BrState { bsCurrCol :: Int,       -- current column
                           bsBroken  :: a }   -- output text

data Element a = ElWord a     -- things we need to place
               | ElSpace Int Bool  -- n of spaces, presence of final breakline
             deriving (Show)

---------------
-- FUNCTIONS --
---------------

-- | Breaks some text to make it fit in a certain width. The output
-- is a Text, suitable for writing to screen or file.
breakText :: StringLike a => BreakFormat -> a -> a
breakText bf cs = out
    where els = parseEls (subTabs (bfTabRep bf) cs)
          out = bsBroken $ foldl (putElem bf) (BrState 0 empty) els

-- | Convenience for @lines $ breakText bf cs@
breakTextLn :: StringLike a => BreakFormat -> a -> [a]
breakTextLn bf cs = lines $ breakText bf cs


-----------------
-- ANCILLARIES --
-----------------

-- PARSING --

-- fino a qui
-- o word 'till ws
-- o wspa 'till (\n | word). se \n, prendilo
parseEls :: StringLike a => a -> [Element a]
parseEls cs = case uncons cs of
  Nothing    -> []
  Just (c,_) | isSpace c -> let (p, r) = span isSpace cs
                            in parseWS p ++ parseEls r
             | otherwise -> let (p, r) = span (not . isSpace) cs
                            in parseWord p : parseEls r

-- Signatures between the two |parse| are different because there can
-- be more element in a single white-space text (newline newline), while
-- that is not possible with parseWord
parseWS :: StringLike a => a -> [Element a]
parseWS ws | null ws   = []
           | otherwise = case span (/= '\n') ws of
               (a, as) | null as   -> [elspace a False] -- no newlines
                       | otherwise -> elspace a True : parseWS (tail as)
    where elspace cs b = ElSpace (length cs) b

parseWord :: StringLike a => a -> Element a
parseWord wr = ElWord wr

-- number of spaces to replace \t with, text
subTabs :: StringLike a => Int -> a -> a
subTabs i cs = f i `concatMap` cs
    where f n c | c == '\t' = pack (replicate i ' ')
                | otherwise = singleton c

-- COMPUTATION --

putElem :: StringLike a => BreakFormat -> BrState a -> Element a -> BrState a
putElem (BreakFormat maxc _ sym hyp)
        bs@(BrState currc currstr) el =
            if avspace >= elLenght el
              then putText bs maxc (el2text el)
              else case el of
                     (ElSpace _ _) -> putText bs maxc (singleton '\n')
                     (ElWord cs)   -> putText bs maxc (broken cs)
    where avspace = maxc - currc -- starting col: 1
          fstcol  = currc == 0
          broken cs = breakWord hyp sym avspace cs fstcol

elLenght :: StringLike a => Element a -> Int
elLenght (ElWord cs)   = length cs
elLenght (ElSpace i _) = i

-- convert element to text
el2text :: StringLike a => Element a -> a
el2text (ElSpace i False) = pack (replicate i ' ')
el2text (ElSpace i True)  = singleton '\n'
el2text (ElWord cs)       = cs

-- put a text and updates the state
-- (more than macol? new line, but no hyphenation!)
putText :: StringLike a => BrState a -> Int -> a -> BrState a
putText bs@(BrState currc currstr) maxcol ts =
  case uncons ts of
    Nothing     -> bs
    Just (c,cs) ->
      let currc' = if c == '\n'
                     then 0
                     else currc + 1
          bs' = if currc' <= maxcol
                  then BrState currc' (currstr `append` singleton c)
                  else BrState 1      (currstr `append` singleton '\n' `append` singleton c)
      in putText bs' maxcol cs

-- breaks a word given remaining space, using an hypenator
-- the last bool is a "you are on the first col, can't start
-- a new line
breakWord :: StringLike a => Maybe Hyphenator -> Char -> Int -> a -> Bool -> a
breakWord mhy ch avspace cs nlb = case find ((<= avspace) . hypLen) poss of
                                    Just a  -> a
                                    Nothing -> cs -- don't find? return input
    where hw = case mhy of
                 Just hy -> map pack (hyphenate hy (unpack cs)) -- :(
                 Nothing -> [cs]
          poss = map cf $ reverse $ zip (inits hw) (tails hw)

          -- crea hyphenated from two bits
          cf ([], ew) = (if nlb then empty else singleton '\n') `append` concat ew
          cf (iw, []) = concat iw `append` singleton '\n'
          cf (iw, ew) = concat iw `append` singleton ch `append` singleton '\n' `append` concat ew

          hypLen cs = length . takeWhile (/= '\n') $ cs

-- CLEAN --

-- removes eof/eol whitespace
hackClean :: StringLike a => a -> a
hackClean cs = noEoflWs cs
  where
  noEoflWs cs = f empty cs

  -- the ugliness
  f acc cs = case uncons cs of
    Nothing     -> acc
    Just (a,as) ->
             let (i,e)  = span (== ' ') cs in
             if null i
               then f (acc `snoc` a) as
               else case uncons e of
                 Just ('\n',rest) -> f (acc `snoc` '\n') rest  -- eol ws
                 Nothing          -> f acc               empty -- eof ws
                 _                -> f (acc `snoc` a)    as    -- normal
