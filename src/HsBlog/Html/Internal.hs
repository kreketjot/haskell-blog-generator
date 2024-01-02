module HsBlog.Html.Internal where


import           Numeric.Natural                ( Natural )

-- * Types

newtype Html = Html String

newtype Structure = Structure String

instance Semigroup Structure where
  (<>) (Structure a) (Structure b) = Structure (a <> b)

instance Monoid Structure where
  mempty = empty_

type Title = String

-- * EDSL (embedded domain specific language)

html_ :: Title -> Structure -> Html
html_ title (Structure content) =
  Html (el "html" (el "head" (el "title" (escape title)) <> el "body" content))

p_ :: String -> Structure
p_ = Structure . el "p" . escape

h_ :: Natural -> String -> Structure
h_ lvl = Structure . el ("h" <> show lvl) . escape

ul_ :: [Structure] -> Structure
ul_ = list "ul"

ol_ :: [Structure] -> Structure
ol_ = list "ol"

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

empty_ :: Structure
empty_ = Structure ""

-- * Render

render :: Html -> String
render html = case html of
  Html str -> str

-- * Utilities

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

getStructureString :: Structure -> String
getStructureString struct = case struct of
  Structure str -> str

escape :: String -> String
escape =
  let escapeChar c = case c of
        '<'  -> "&lt;"
        '>'  -> "&gt;"
        '&'  -> "&amp;"
        '"'  -> "&quot;"
        '\'' -> "&#39;"
        _    -> [c]
  in  concatMap escapeChar

list :: String -> [Structure] -> Structure
list tag =
  let wrapLi (Structure x) = el "li" x in Structure . el tag . concatMap wrapLi

concatStructures :: [Structure] -> Structure
concatStructures structures = case structures of
  []     -> empty_
  x : xs -> x <> concatStructures xs
