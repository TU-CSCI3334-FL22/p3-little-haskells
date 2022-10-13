import System.IO
import Control.Monad

data Token = SEMICOLON | DERIVES | ALSODERIVES | EPSILON | SYMBOL String deriving (Show, Eq)

main = do
        contents <- readFile "grammars/demo.gram"
        let myLines = words contents
        map putStr myLines


readToken str
  | str == ";" = SEMICOLON
