module Html.Internal where

-- * Types

newtype Html = Html String

newtype Structure = Structure String

type Title = String

-- * EDSL (embedded domain specific language)

html_ :: Title -> Structure -> Html
html_ title (Structure content) =
  Html
    ( el
        "html"
        ( el
            "head"
            (el "title" (escape title))
            <> el "body" content
        )
    )

p_ :: String -> Structure
p_ = Structure . el "p" . escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

append_ :: Structure -> Structure -> Structure
append_ (Structure a) (Structure b) = Structure (a <> b)

-- * Render

render :: Html -> String
render html =
  case html of
    Html str -> str

-- * Utilities

getStructureString :: Structure -> String
getStructureString struct =
  case struct of
    Structure str -> str

escape :: String -> String
escape =
  let escapeChar c =
        case c of
          '<' -> "&lt;"
          '>' -> "&gt;"
          '&' -> "&amp;"
          '"' -> "&quot;"
          '\'' -> "&#39;"
          _ -> [c]
   in concat . map escapeChar
