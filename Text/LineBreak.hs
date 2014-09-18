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
-- Simple functions to break a String to fit a maximum text width, using
-- Knuth-Liang hyphenation algorithm.
--
-- Simple example usage:
-- >>> let hyp = Just english_US
-- >>> let bf = BreakFormat 25 4 '-' hyp
-- >>> let cs = "Using hyphenation with gruesomely non parsimonious wording."
-- >>> putStr $ breakString bf cs
-- Using hyphenation with
-- gruesomely non parsimo-
-- nious wording.
--
-------------------------------------------------------------------------------

module Text.LineBreak ( breakString, breakStringLn, BreakFormat(..) ) where

import Text.Hyphenation
import Data.Char (isSpace)
import Data.List (find, inits, tails, span)


-----------
-- TYPES --
-----------

-- | How to break the strings: maximum width of the lines, number of spaces
-- to replace tabs with (dumb replacement), symbol to use to hyphenate
-- words, hypenator to use (language, exceptions, etc.; refer to
-- "Text.Hyphenation" for usage instructions). To break lines without
-- hyphenating, put @Nothing@ in @bfHyphenator@.
data BreakFormat = BreakFormat { bfMaxCol :: Int,
                                 bfTabRep :: Int,
                                 bfHyphenSymbol :: Char,
                                 bfHyphenator :: Maybe Hyphenator }

data BrState = BrState { bsCurrCol :: Int,       -- current column
                         bsBroken  :: String }   -- output string

data Element = ElWord String     -- things we need to place
             | ElSpace Int Bool  -- n of spaces, presence of final breakline
             deriving (Show)

---------------
-- FUNCTIONS --
---------------

-- | Breaks some text (String) to make it fit in a certain width. The output
-- is a String, suitable for writing to screen or file.
breakString :: BreakFormat -> String -> String
breakString bf cs = hackClean out
    where els = parseEls (subTabs (bfTabRep bf) cs)
          out = bsBroken $ foldl (putElem bf) (BrState 0 "") els

-- | Convenience for @lines $ breakString bf cs@
breakStringLn :: BreakFormat -> String -> [String]
breakStringLn bf cs = lines $ breakString bf cs


-----------------
-- ANCILLARIES --
-----------------

-- PARSING --

-- fino a qui
-- o word 'till ws
-- o wspa 'till (\n | word). se \n, prendilo
parseEls :: String -> [Element]
parseEls []       = []
parseEls cs@(c:_) | isSpace c = let (p, r) = span isSpace cs
                                in parseWS p ++ parseEls r
                  | otherwise = let (p, r) = span (not . isSpace) cs
                                in parseWord p : parseEls r

-- Signatures between the two |parse| are different because there can
-- be more element in a single white-space string (newline newline), while
-- that is not possible with parseWord
parseWS :: String -> [Element]
parseWS [] = []
parseWS ws = case span (/= '\n') ws of
               (a, "")      -> [elspace a False] -- no newlines
               (a, '\n':rs) ->  elspace a True : parseWS rs
    where elspace cs b = ElSpace (length cs) b

parseWord :: String -> Element
parseWord wr = ElWord wr

-- number of spaces to replace \t with, string
subTabs :: Int -> String -> String
subTabs i cs = cs >>= f i
    where f n '\t' = replicate i ' '
          f _ c    = return c

-- COMPUTATION --

putElem :: BreakFormat -> BrState -> Element -> BrState
putElem (BreakFormat maxc _ sym hyp)
        bs@(BrState currc currstr) el =
            if avspace >= elLenght el
              then putString bs maxc (el2string el)
              else case el of
                     (ElSpace _ _) -> putString bs maxc "\n"
                     (ElWord cs)   -> putString bs maxc (broken cs)
    where avspace = maxc - currc -- starting col: 1
          fstcol  = currc == 0
          broken cs = breakWord hyp sym avspace cs fstcol

elLenght :: Element -> Int
elLenght (ElWord cs)   = length cs
elLenght (ElSpace i _) = i

-- convert element to string
el2string :: Element -> String
el2string (ElSpace i False) = replicate i ' '
el2string (ElSpace i True) = "\n"
el2string (ElWord cs) = cs

-- put a string and updates the state
-- (more than macol? new line, but no hyphenation!)
putString :: BrState -> Int -> String -> BrState
putString bs                      _      [] = bs
putString (BrState currc currstr) maxcol (c:cs) =
                let currc' = if c == '\n'
                               then 0
                               else currc + 1
                    bs' = if currc' <= maxcol
                            then BrState currc' (currstr ++ [c])
                            else BrState 1      (currstr ++ "\n" ++ [c])
                in putString bs' maxcol cs

-- breaks a word given remaining space, using an hypenator
-- the last bool is a "you are on the first col, can't start
-- a new line
breakWord :: Maybe Hyphenator -> Char -> Int -> String -> Bool -> String
breakWord mhy ch avspace cs nlb = case find ((<= avspace) . hypLen) poss of
                                    Just a  -> a
                                    Nothing -> cs -- don't find? return input
    where hw = case mhy of
                 Just hy -> hyphenate hy cs
                 Nothing -> [cs]
          poss = map cf $ reverse $ zip (inits hw) (tails hw)
            -- poss ~= ["cascata\n","cas-\ncata","casca-\nta","\ncascata"]

          -- crea hyphenated from two bits
          cf ([], ew) = (if nlb then "" else "\n") ++ concat ew
          cf (iw, []) = concat iw ++ "\n"
          cf (iw, ew) = concat iw ++ [ch] ++ "\n" ++ concat ew

          hypLen cs = length . takeWhile (/= '\n') $ cs

-- CLEAN --

-- removes eof/eol whitespace
hackClean :: String -> String
hackClean cs = noEoflWs cs
    where noEoflWs cs = f "" cs

          -- the ugliness
          f acc []        = acc
          f acc cs@(a:as) =
                let (i, e) = span (== ' ') cs in
                if i == ""
                  then f (acc ++ [a]) as
                  else case e of
                         ('\n':rest) -> f (acc ++ "\n") rest -- eol ws
                         []          -> f acc           []   -- eof ws
                         _           -> f (acc ++ [a]) as -- normal


