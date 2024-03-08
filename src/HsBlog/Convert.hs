module HsBlog.Convert
  ( convert
  , convertStructure
  ) where

import qualified HsBlog.Html                   as Html
import qualified HsBlog.Markup                 as Markup

convert :: Html.Title -> Markup.Document -> Html.Html
convert title = Html.html_ title . foldMap convertStructure

convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure = case structure of
  Markup.Heading level content -> Html.h_ level $ Html.txt_ content
  Markup.Paragraph     content -> Html.p_ $ Html.txt_ content
  Markup.UnorderedList list    -> Html.ul_ $ map (Html.p_ . Html.txt_) list
  Markup.OrderedList   list    -> Html.ol_ $ map (Html.p_ . Html.txt_) list
  Markup.CodeBlock     list    -> Html.code_ (unlines list)
