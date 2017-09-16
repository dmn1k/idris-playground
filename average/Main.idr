module Main

import Average

showAverage : String -> String
showAverage str = show (average str) ++ "\n"

main : IO ()
main = repl "> " showAverage

