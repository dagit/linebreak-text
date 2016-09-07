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

import           Text.Hyphenation
import           Data.Char ( isSpace )
import           Data.List ( inits, tails, find )
import           Data.Monoid ( (<>) )
import qualified Data.Text as T
import           Data.Text ( Text )

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

data BrState = BrState { bsCurrCol :: Int,       -- current column
                         bsBroken  :: Text }   -- output text

data Element = ElWord Text     -- things we need to place
             | ElSpace Int Bool  -- n of spaces, presence of final breakline
             deriving (Show)

---------------
-- FUNCTIONS --
---------------

-- | Breaks some text to make it fit in a certain width. The output
-- is a Text, suitable for writing to screen or file.
breakText :: BreakFormat -> Text -> Text
breakText bf cs = out
    where els = parseEls (subTabs (bfTabRep bf) cs)
          out = bsBroken $ foldl (putElem bf) (BrState 0 "") els

-- | Convenience for @lines $ breakText bf cs@
breakTextLn :: BreakFormat -> Text -> [Text]
breakTextLn bf cs = T.lines $ breakText bf cs


-----------------
-- ANCILLARIES --
-----------------

-- PARSING --

-- fino a qui
-- o word 'till ws
-- o wspa 'till (\n | word). se \n, prendilo
parseEls :: Text -> [Element]
parseEls cs = case T.uncons cs of
  Nothing    -> []
  Just (c,_) | isSpace c -> let (p, r) = T.span isSpace cs
                            in parseWS p ++ parseEls r
             | otherwise -> let (p, r) = T.span (not . isSpace) cs
                            in parseWord p : parseEls r

-- Signatures between the two |parse| are different because there can
-- be more element in a single white-space text (newline newline), while
-- that is not possible with parseWord
parseWS :: Text -> [Element]
parseWS ws | T.null ws = []
           | otherwise = case T.span (/= '\n') ws of
               (a, as) | T.null as -> [elspace a False] -- no newlines
                       | otherwise -> elspace a True : parseWS (T.tail as)
    where elspace cs b = ElSpace (T.length cs) b

parseWord :: Text -> Element
parseWord wr = ElWord wr

-- number of spaces to replace \t with, text
subTabs :: Int -> Text -> Text
subTabs i cs = f i `T.concatMap` cs
    where f n c | c == '\t' = T.pack (replicate i ' ')
                | otherwise = T.singleton c

-- COMPUTATION --

putElem :: BreakFormat -> BrState -> Element -> BrState
putElem (BreakFormat maxc _ sym hyp)
        bs@(BrState currc currstr) el =
            if avspace >= elLenght el
              then putText bs maxc (el2text el)
              else case el of
                     (ElSpace _ _) -> putText bs maxc "\n"
                     (ElWord cs)   -> putText bs maxc (broken cs)
    where avspace = maxc - currc -- starting col: 1
          fstcol  = currc == 0
          broken cs = breakWord hyp sym avspace cs fstcol

elLenght :: Element -> Int
elLenght (ElWord cs)   = T.length cs
elLenght (ElSpace i _) = i

-- convert element to text
el2text :: Element -> Text
el2text (ElSpace i False) = T.pack (replicate i ' ')
el2text (ElSpace i True) = "\n"
el2text (ElWord cs) = cs

-- put a text and updates the state
-- (more than macol? new line, but no hyphenation!)
putText :: BrState -> Int -> Text -> BrState
putText bs@(BrState currc currstr) maxcol ts =
  case T.uncons ts of
    Nothing     -> bs
    Just (c,cs) ->
      let currc' = if c == '\n'
                     then 0
                     else currc + 1
          bs' = if currc' <= maxcol
                  then BrState currc' (currstr <> T.singleton c)
                  else BrState 1      (currstr <> "\n" <> T.singleton c)
      in putText bs' maxcol cs

-- breaks a word given remaining space, using an hypenator
-- the last bool is a "you are on the first col, can't start
-- a new line
breakWord :: Maybe Hyphenator -> Char -> Int -> Text -> Bool -> Text
breakWord mhy ch avspace cs nlb = case find ((<= avspace) . hypLen) poss of
                                    Just a  -> a
                                    Nothing -> cs -- don't find? return input
    where hw = case mhy of
                 Just hy -> map T.pack (hyphenate hy (T.unpack cs)) -- :(
                 Nothing -> [cs]
          poss = map cf $ reverse $ zip (inits hw) (tails hw)

          -- crea hyphenated from two bits
          cf ([], ew) = (if nlb then "" else "\n") <> T.concat ew
          cf (iw, []) = T.concat iw <> "\n"
          cf (iw, ew) = T.concat iw <> T.singleton ch <> "\n" <> T.concat ew

          hypLen cs = T.length . T.takeWhile (/= '\n') $ cs

-- CLEAN --

-- removes eof/eol whitespace
hackClean :: Text -> Text
hackClean cs = noEoflWs cs
  where
  noEoflWs cs = f "" cs

  -- the ugliness
  f acc cs = case T.uncons cs of
    Nothing     -> acc
    Just (a,as) ->
             let (i,e)  = T.span (== ' ') cs in
             if i == ""
               then f (acc `T.snoc` a) as
               else case T.uncons e of
                 Just ('\n',rest) -> f (acc `T.snoc` '\n') rest    -- eol ws
                 Nothing          -> f acc                 T.empty -- eof ws
                 _                -> f (acc `T.snoc` a)    as      -- normal
