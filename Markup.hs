module Markup
  ( Document,
    Structure (..),
    parse,
  )
where

import Numeric.Natural (Natural)

type Document = [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]

parse :: String -> Document
parse = parseLines [] . lines

parseLines :: [String] -> [String] -> Document
parseLines currentParagraph linesToRead =
  let paragraph = Paragraph (unwords (reverse currentParagraph))
   in case linesToRead of
        [] -> [paragraph]
        (currentLine : restLines) ->
          if trim currentLine == ""
            then paragraph : parseLines [] restLines
            else parseLines (currentLine : currentParagraph) restLines

trim :: String -> String
trim = unwords . words
