import Html

main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_
    "Hello title"
    ( append_
        (h1_ "Hello, world!")
        ( append_
            (p_ "Some <p>'p' tag</p>")
            ( ul_
                [ p_ "Another paragraph",
                  ol_
                    [ code_ "code 1",
                      code_ "code      2"
                    ]
                ]
            )
        )
    )
