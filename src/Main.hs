{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where

import Control.Applicative
import System.Environment
import Flow
import Data.Maybe (catMaybes)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import qualified Text.CSS.Parse as Parse

import Data.Aeson
import GHC.Generics

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Network.HTTP.Simple

import Control.Concurrent (threadDelay)

type CssStyle = (String, String)
type KeyFrame = Int
data Animation = Animation String [(KeyFrame, [CssStyle])]
                 deriving (Show)

newtype SourceDirectory = SourceDirectory{name :: Text} deriving (Show, Generic)
newtype SourceFile      = SourceFile{download_url :: Text} deriving (Show, Generic)

instance FromJSON SourceDirectory
instance FromJSON SourceFile

main :: IO ()
main = do
  args <- getArgs
  initReq <- parseRequest <| head args
  let req = addRequestHeader "User-Agent" "Css-to-Elm" initReq
  response <- httpJSONEither req :: IO (Response (Either JSONException [SourceDirectory]))
  case getResponseBody response of
    Left err -> print err
    Right sourceDirectories -> processDirectories (head args) (map (getDirectoryUrl <| head args) sourceDirectories)

processDirectories :: String -> [String] -> IO ()
processDirectories baseUrl (directory : restOfDirectories) =
  processDirectory directory >> processDirectories baseUrl restOfDirectories
  where
    processDirectory url = do
      print url
      initReq <- parseRequest url
      let req = addRequestHeader "User-Agent" "Css-to-Elm" initReq
      result <- httpJSONEither req :: IO (Response (Either JSONException [SourceFile]))
      case getResponseBody result of
        Left err -> print err
        Right sourceFileUrls -> processFiles baseUrl sourceFileUrls

processFiles :: String -> [SourceFile] -> IO ()
processFiles baseUrl (file : restOfFiles) =
  processFile file >> processFiles baseUrl restOfFiles
  where
    processFile file = do
      print file
      initReq <- parseRequest <| unpack (download_url file)
      let req = addRequestHeader "User-Agent" "Css-to-Elm" initReq
      result <- httpBS req
      let cssText = decodeUtf8 <| getResponseBody result
      parseResult <- parseText cssText
      let animations = catMaybes <| mapTreeToElm parseResult
      putStrLn <| show animations
      threadDelay 100000


getDirectoryUrl :: String -> SourceDirectory -> String
getDirectoryUrl baseUrl sourceDirectory =
  baseUrl ++ "/" ++ (unpack <| name sourceDirectory)

parseText :: Text -> IO [Parse.NestedBlock]
parseText text =
  parseResult <| Parse.parseNestedBlocks text
  where
    parseResult (Left err) =
      error err
    parseResult (Right result) =
      return result

mapTreeToElm :: [Parse.NestedBlock] -> [Maybe Animation]
mapTreeToElm (block : remainingBlocks) =
  animationResult block : mapTreeToElm remainingBlocks
  where
    animationResult (Parse.LeafBlock _) =
      Nothing
    animationResult (Parse.NestedBlock text nestedBlocks) =
      Just $ Animation (unpack text) (catMaybes <| map leafBlockResult nestedBlocks)
    leafBlockResult (Parse.LeafBlock cssBlock) =
      cssMapping cssBlock
    cssMapping (keyFrame, [(a, b)]) =
      case unpack keyFrame of
        "from" -> Just (0, [(unpack a, unpack b)])
        "to" -> Just (100, [(unpack a, unpack b)])
        _ ->
          case readMaybe <| unpack keyFrame of
            Just n -> Just (n, [(unpack a, unpack b)])
            Nothing -> Nothing

mapTreeToElm [] =
  []

-- converts an input `String` (i think) into a `Maybe a`.
-- in this example i use it to get a "Maybe Int" from the user's input.
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
                  [(val, "")] -> Just val
                  _           -> Nothing
