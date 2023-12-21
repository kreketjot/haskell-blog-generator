import Html

main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_
    "Hello title"
    ( h1_ "Hello, world!"
        <> p_ "Some <p>'p' tag</p>"
        <> ul_
          [ p_ "Another paragraph",
            ol_
              [ code_ "code 1",
                code_ "code      2"
              ]
          ]
    )
