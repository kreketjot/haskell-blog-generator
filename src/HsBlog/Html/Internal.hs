module HsBlog.Html.Internal where


import           Numeric.Natural                ( Natural )

-- * Types

newtype Html = Html String

newtype Structure = Structure String

newtype Content = Content String

type Title = String

-- * EDSL (embedded domain specific language)

html_ :: Title -> Structure -> Html
html_ title (Structure content) =
  Html (el "html" (el "head" (el "title" (escape title)) <> el "body" content))

p_ :: Content -> Structure
p_ = Structure . el "p" . getContentString

h_ :: Natural -> Content -> Structure
h_ lvl = Structure . el ("h" <> show lvl) . getContentString

ul_ :: [Structure] -> Structure
ul_ = list "ul"

ol_ :: [Structure] -> Structure
ol_ = list "ol"

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

txt_ :: String -> Content
txt_ = Content . escape

link_ :: FilePath -> Content -> Content
link_ href (Content innerContent) = Content
  $ element "a" [("href", "\"" <> escape href <> "\"")] (Just innerContent)

img_ :: FilePath -> String -> Content
img_ src alt = Content $ element
  "img"
  [("src", "\"" <> escape src <> "\""), ("alt", "\"" <> escape alt <> "\"")]
  Nothing

b_ :: Content -> Content
b_ = Content . el "b" . getContentString

i_ :: Content -> Content
i_ = Content . el "i" . getContentString

empty_ :: Structure
empty_ = Structure ""

-- * Render

render :: Html -> String
render html = case html of
  Html str -> str

-- * Structure

instance Semigroup Structure where
  (<>) (Structure a) (Structure b) = Structure (a <> b)

instance Monoid Structure where
  mempty = empty_

getStructureString :: Structure -> String
getStructureString struct = case struct of
  Structure str -> str

-- * Content

instance Semigroup Content where
  (<>) (Content c1) (Content c2) = Content (c1 <> c2)

instance Monoid Content where
  mempty = Content ""

getContentString :: Content -> String
getContentString (Content str) = str

-- * Utilities

el :: String -> String -> String
el tag content = element tag [] (Just content)

element :: String -> [(String, String)] -> Maybe String -> String
element tag attributes mContent =
  let begin =
        "<"
          <> tag
          <> foldr
               (\(name, value) attrs -> attrs <> " " <> name <> "=" <> value)
               ""
               attributes
      end = case mContent of
        Nothing        -> " />"
        (Just content) -> ">" <> content <> "</" <> tag <> ">"
  in  begin <> end

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
