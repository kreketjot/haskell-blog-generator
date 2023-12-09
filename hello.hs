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
            (p_ "Some text")
            (p_ "Another paragraph")
        )
    )
