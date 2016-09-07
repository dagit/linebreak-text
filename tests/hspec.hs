{-# LANGUAGE OverloadedStrings #-}
module Main(main) where
import           Test.Hspec
import           Text.LineBreak
import           Text.Hyphenation
import qualified Data.Text as T
import           Data.Monoid ( (<>) )


hyp = Just english_US
bf = BreakFormat 25 4 '-' hyp
testBr = breakText bf
myUnlines = T.init . T.unlines -- unlines adds an extra "\n" ad end-of-string


main :: IO ()
main = hspec $ do

  describe "Text.Linebreak.breakText" $ do

    let sip    = "Using hyphenation with gruesomely non parsimonious wording."
        sipout = myUnlines ["Using hyphenation with",
                            "gruesomely non parsimo-",
                            "nious wording." ]
    it "breaks a text to certain width" $
      testBr sip `shouldBe` sipout

    let sipoutNH = myUnlines ["Using hyphenation with",
                              "gruesomely non",
                              "parsimonious wording." ]
        bfnh = BreakFormat 25 4 '-' Nothing
    it "breaks a text to certain width (w/o hypenator)" $
      breakText bfnh sip `shouldBe` sipoutNH

    let postws    = "word      "
        postwsout = "word"
    it "ws at end of word should be truncated" $
      testBr postws `shouldBe` postwsout

    let postwscc    = "aaa" <> T.replicate (25 - 3 - 1) (T.singleton ' ') <> "bbb"
        postwsccout = "aaa\nbbb"
    it "no end-of-line white-space, corner case (words added after it)" $
      testBr postwscc `shouldBe` postwsccout

    let broken    = "broken\nstring"
        brokenout = myUnlines ["broken", "string"]
    it "should not remove \\n already in the string" $
      testBr broken `shouldBe` brokenout

    let spaced    = "spaced   string"
        spacedout = spaced
    it "should not remove blankspace already in the string" $
      testBr spaced `shouldBe` spacedout

    let simple    = "simple"
        simpleout = simple
    it "should not add extra \\n at the end of the string" $
      testBr simple `shouldBe` simpleout

    let prews    = "     word"
        prewsout = prews
    it "should not remove whitespace @ beginning of string" $
      testBr prews `shouldBe` prewsout

    let muchws    =          "word                              second"
        muchwsout = myUnlines ["word", "second"]
    it "should truncate eol whitespace" $
      testBr muchws `shouldBe` muchwsout

    let toolong    = T.replicate 24 (T.singleton 'a') <> "bbb"
        toolongout = T.replicate 24 (T.singleton 'a') <> "b\nbb"
    it "should truncate words longer than screenwidth" $
      testBr toolong `shouldBe` toolongout

    -- TODO what happens with really tiny width?
    let tinywd    = "tomcat"
        tinywdout = "to\nmc\nat"
        tinybf = BreakFormat 2 4 '-' (Just english_US)
        tinyBr = breakText tinybf
    it "should wrap (but not hyphenate long words)" $
      tinyBr tinywd `shouldBe` tinywdout


  describe "Text.Linebreak.breakTextLn" $ do

    let sip = "Using hyphenation with gruesomely non parsimonious wording."
    it "is the same as |lines $ breakText bf cs|" $ do
      breakTextLn bf sip `shouldBe` (T.lines $ breakText bf sip)

