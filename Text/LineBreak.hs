module Text.LineBreak (breakString, BreakFormat (..)) where

import Data.List (intercalate)
import Text.Hyphenation


-- TYPES --

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


-- | TODO
breakString :: BreakFormat -> String -> String
breakString bf para =
        let ls = lines para in
        unlines $ map (putLine "" bf) ls

