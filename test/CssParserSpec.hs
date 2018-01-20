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
  _ <- commentSpec
  _ <- whiteSpaceSpec
  identTokenSpec


commentSpec :: Spec
commentSpec = do
  describe "comment - success cases" $ do
    it "successfully parses a single line commnet - /*comment*/ into ()" $
      ("/*comment*/" :: Text) ~> comment
        `shouldParse` ()
    it "successfully parses a multiline comment - /*\ncomment\n*/ into ()" $
      ("/*\ncomment\n*/" :: Text) ~> comment
        `shouldParse` ()
    it "successfully parses a comment with *s - /**********/ into ()" $
      ("/**********/" :: Text) ~> comment
        `shouldParse` ()

  describe "comment - failing cases" $ do
    it "fails to parse an unclosed comment - /*comment" $
      comment `shouldFailOn` ("/*comment" :: Text)
    it "fails to parse an unopened comment - comment*/" $
      comment `shouldFailOn` ("comment*/" :: Text)

whiteSpaceSpec :: Spec
whiteSpaceSpec = do
  describe "whiteSpace - success cases" $ do
    it "successfully parses spaces - \\s+ into ()" $
      ("      " :: Text) ~> whiteSpace
        `shouldParse` ()
    it "successfully parses spaces and newlines - \\s+\\n+ into ()" $
      ("      \n\n\n\n" :: Text) ~> whiteSpace
        `shouldParse` ()


identTokenSpec :: Spec
identTokenSpec = do
  describe "identToken - success cases" $ do
    it "successfully parses an ident beginning with \"-\" -webkit-aspect-ratio" $
      ("-webkit-aspect-ratio" :: Text) ~> identToken
        `shouldParse` "-webkit-aspect-ratio"
    it "successfully parses an ident beginning with lower case alpha - ident-lower" $
      ("ident-lower" :: Text) ~> identToken
        `shouldParse` "ident-lower"
    it "successfully parses an ident beginning with upper case alpha - Ident-upper" $
      ("Ident-upper" :: Text) ~> identToken
        `shouldParse` "Ident-upper"

  describe "identToken - failing cases" $ do
    it "fails to parse two consecutive hyphens at the start --webkit-" $
      identToken `shouldFailOn` ("--webkit-" :: Text)