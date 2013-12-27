--------------------------------------------------------------------------------
-- |
-- Module      :  Text.LineBreak
-- Copyright   :  (C) 2013 Francesco Ariis
-- License     :  BSD3 (see LICENSE file)
--
-- Maintainer  :  Francesco Ariis <fa-ml@ariis.it>
-- Stability   :  provisional
-- Portability :  portable
--
-- Simple functions to break a String to fit a maximum text width, using
-- Knuth-Lian hyphenation algorithm.
--
-- Example:
--
-- > import Text.Hyphenation
-- > import Text.LineBreak
-- >
-- > hyp = Just english_US
-- > bf = BreakFormat 25 '-' hyp
-- > cs = "Using hyphenation with gruesomely non parsimonious wording."
-- >
-- > main = putStr $ breakString bf cs
--
-- will output:
--
-- > Using hyphenation with
-- > gruesomely non parsimo-
-- > nious wording.
--
-------------------------------------------------------------------------------

module Text.LineBreak ( breakString, breakStringLn, BreakFormat (..) ) where

import Data.List (intercalate)
import Text.Hyphenation

-- TODO: what to do in overflow case?

-- @
--   let hyp = Just english_US in
--   putStr $ breakString (BreakFormat 55 '-' hyp) str2
-- @
--
-- @
--   Mathematicians seek out patterns and use them to formu-
--   late new conjectures. Mathematicians resolve the truth
--   or falsity of conjectures by mathematical proof.
-- @

-- TYPES --

-- | How to break the Strings: maximum width of the lines, symbol to use
-- to hyphenate a word, Hypenator to use (language, exceptions, etc. Refer to
-- "Text.Hyphenation" for more info). To break lines without hyphenating, put
-- @Nothing@ in @bfHyphenator@.
data BreakFormat = BreakFormat { bfMaxCol :: Int,
                                 bfHyphenSymbol :: Char,
                                 bfHyphenator :: Maybe Hyphenator }


-- bit: hyphenated part of a word
data WordPart = Init | Mid | End | Single
                deriving (Eq, Show)

isInit Init   = True
isInit Single = True
isInit _      = False

isEnd End    = True
isEnd Single = True
isEnd _      = False


putBit :: String -> BreakFormat -> WordPart -> String -> String
putBit oldcs (BreakFormat maxcol hypsym _) wp bit =
        let
            spaceleft =
                maxcol - currcol +
                (if isInit wp then (-1) else 0) +    -- new world
                (if not $ isEnd wp then (-1) else 0) -- possible hyp. space
            lenbit = length bit

            addbefore
                  -- begin of word, no space left
                | isInit wp && lenbit > spaceleft = "\n"   -- put newline
                  -- begin of word, not @ first column
                | isInit wp && currcol /= 0 = " "          -- put space
                   -- not begin of word, need to newline
                | not (isInit wp) &&
                  lenbit > spaceleft = hypsym : "\n"       -- put hyphens\n
                   -- nothing special
                | otherwise = ""                           -- put nothing
        in

        oldcs ++ addbefore ++ bit
    where currcol = length . last . lines $ ('\n' : oldcs)


putWord :: String -> BreakFormat -> String -> String
putWord oldcs bf@(BreakFormat _ _ mhyp) word =
        let bit = case mhyp of
                    (Just hyp) -> hyphenate hyp word
                    Nothing    -> [word]
        in

        -- There is a possible optimisation here (check if the word is bigger
        -- than the remaining space and pass it as a singleton. Since there
        -- wasn't a notable gain in time, I scrapped it.

        case bit of
          (ba:[])    -> putBit oldcs bf Single ba               -- singleton
          (ba:bb:[]) -> let newcs = putBit oldcs bf Init ba in
                        putBit newcs bf End bb                  -- pair
          (ba:bs)    -> let inics = putBit oldcs bf Init ba     -- triplet+
                            bodcs = foldl f inics (init bs)  in
                        putBit bodcs bf End (last bs)
    where f oldcs bit = putBit oldcs bf Mid bit


putLine :: String -> BreakFormat -> String -> String
putLine oldcs bf line =
        let ws = words line in
        foldl f oldcs ws
    where f oldcs w = putWord oldcs bf w


-- | Breaks a String to make it fit in a certain width. The output is a String,
-- suitable for writing to screen or file.
breakString :: BreakFormat -> String -> String
breakString bf para = unlines $ breakStringLn bf para

-- | Convenience for @lines $ breakString bf cs@
breakStringLn :: BreakFormat -> String -> [String]
breakStringLn bf para =
        let ls = lines para in
        map (putLine "" bf) ls

