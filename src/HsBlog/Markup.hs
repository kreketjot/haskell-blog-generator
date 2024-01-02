module HsBlog.Markup
  ( Document
  , Structure(..)
  , parse
  ) where

import           Data.Maybe                     ( maybeToList )
import           Numeric.Natural                ( Natural )

type Document = [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving (Show, Eq)

parse :: String -> Document
parse = parseLines Nothing . lines

parseLines :: Maybe Structure -> [String] -> Document
parseLines context linesToRead = case linesToRead of
    -- done case
  [] -> maybeToList context
  -- Heading 1 case
  ('*' : ' ' : line) : restLines ->
    maybe id (:) context (parseLines (Just (Heading 1 (trim line))) restLines)
  -- Unordered list case
  ('-' : ' ' : line) : restLines -> case context of
    Just (UnorderedList list) ->
      parseLines (Just (UnorderedList (list <> [trim line]))) restLines
    _ -> maybe id
               (:)
               context
               (parseLines (Just (UnorderedList [trim line])) restLines)
  -- Ordered list case
  ('#' : ' ' : line) : restLines -> case context of
    Just (OrderedList list) ->
      parseLines (Just (OrderedList (list <> [trim line]))) restLines
    _ -> maybe id
               (:)
               context
               (parseLines (Just (OrderedList [trim line])) restLines)
  -- CodeBlock case
  ('>' : ' ' : line) : restLines -> case context of
    Just (CodeBlock codeLines) ->
      parseLines (Just (CodeBlock (codeLines <> [line]))) restLines
    _ -> maybe id (:) context (parseLines (Just (CodeBlock [line])) restLines)
  -- Paragraph case
  currentLine : restLines ->
    let line = trim currentLine
    in
      if line == ""
        then maybe id (:) context (parseLines Nothing restLines)
        else case context of
          Just (Paragraph paragraph) ->
            parseLines (Just (Paragraph (unwords [paragraph, line]))) restLines
          _ ->
            maybe id (:) context (parseLines (Just (Paragraph line)) restLines)

trim :: String -> String
trim = unwords . words
