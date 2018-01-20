{-# LANGUAGE OverloadedStrings #-}

module Main where

import CssParser
import Data.Text (Text)

import Test.Hspec
import Test.Hspec.Attoparsec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "CssParser comment - success cases" $ do
      it "successfully parses a single line commnet - /*comment*/ into ()" $
        ("/*comment*/" :: Text) ~> comment
          `shouldParse` ()
      it "successfully parses a multiline comment - /*\ncomment\n*/ into ()" $
        ("/*\ncomment\n*/" :: Text) ~> comment
          `shouldParse` ()
      it "successfully parses a comment with *s - /**********/ into ()" $
        ("/**********/" :: Text) ~> comment
          `shouldParse` ()

    describe "CssParser comment - failing cases" $ do
      it "fails to parse an unclosed comment - /*comment" $
        comment `shouldFailOn` ("/*comment" :: Text)
      it "fails to parse an unopened comment - comment*/" $
        comment `shouldFailOn` ("comment*/" :: Text)


