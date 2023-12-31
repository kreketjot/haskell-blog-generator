module HsBlog.Convert
  ( convert
  ) where

import qualified HsBlog.Html                   as Html
import qualified HsBlog.Markup                 as Markup

convert :: Html.Title -> Markup.Document -> Html.Html
convert title = Html.html_ title . foldMap convertStructure

convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure = case structure of
  Markup.Heading level content -> Html.h_ level content
  Markup.Paragraph     content -> Html.p_ content
  Markup.UnorderedList list    -> Html.ul_ $ map Html.p_ list
  Markup.OrderedList   list    -> Html.ol_ $ map Html.p_ list
  Markup.CodeBlock     list    -> Html.code_ (unlines list)
