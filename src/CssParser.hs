{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module CssParser where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Char
import           Data.Text            hiding (concat, takeWhile)
import           Prelude              hiding (takeWhile)

import qualified Debug.Trace          as Trace

data CssRule
  = QualifiedRule
  | AtRule
  deriving (Show)

newtype Stylesheet =
  Stylesheet [CssRule]
  deriving (Show)

-- Lexer rules
comment :: Parser ()
comment = (string "/*" *> manyTill anyChar (string "*/")) >> return ()

whiteSpace :: Parser ()
whiteSpace = many (comment <|> (skip isSpace >> skipWhile isSpace)) >> return ()

identToken :: Parser String
identToken = do
  hyphen <- option "" (char '-' >> return "-")
  lead <- satisfy (\c -> c /= '-')
  restT <- (takeWhile (\x -> isPrint x || x == '_'))
  let rest = unpack restT
  return $ hyphen ++ lead : rest

quotedString :: Parser String
quotedString = do
  _ <- char '"'
  content <- many character
  _ <- char '"'
  return $ concat content
  where
    escape = do
      d <- char '\\'
      c <- satisfy $ inClass "\\\"0nrvtbf" -- this may perform poorly
      return [d, c]
    nonEscape = (\x -> [x]) <$> (satisfy $ notInClass "\\\"\0\n\r\v\t\b\f")
    character = nonEscape <|> escape

stylesheet :: Parser Stylesheet
stylesheet = many stylesheetElement >>= \es -> return $ Stylesheet es
  where
    stylesheetElement =
      Trace.trace "stylesheetElement" $ do
        _ <- whiteSpace
        e <- atRule <|> qualifiedRule
        _ <- whiteSpace
        return e

qualifiedRule :: Parser CssRule
qualifiedRule =
  Trace.trace
    ("qualified rule")
    (string "qualifiedRule" >> return QualifiedRule)

atRule :: Parser CssRule
atRule = string "@atRule" >> return AtRule
