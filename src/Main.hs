{-# LANGUAGE OverloadedStrings #-}
module Main where

import Flow
import Data.Maybe (catMaybes)
import Data.Text (Text, pack, unpack)
import qualified Text.CSS.Parse as Parse

type CssStyle = (String, String)
type KeyFrame = Int
data Animation = Animation String [(KeyFrame, [CssStyle])]
                 deriving (Show)

main :: IO ()
main = do
  cssText <- readFile "/Users/jonathanfrankel/projects/playground/css_to_elm/source.css"
  parseResult <- parseText <| pack cssText
  let animations = catMaybes <| mapTreeToElm parseResult
  putStrLn <| show animations

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
