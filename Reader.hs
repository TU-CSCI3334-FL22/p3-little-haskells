module Reader where
import Data.Char

type Terminal = String
type NonTerminal = String
type Symbol = String
type SymbolTable = [(String, Int)] --Map of strings to ints 
data Token = SEMICOLON | DERIVES | ALSODERIVES | EPSILON | SYMBOL String deriving (Show, Eq)
type Production = (NonTerminal, [Symbol])
data IR = IR [Terminal] [NonTerminal] [(Integer, Production)]

isAlphaNumString str = foldl (\res char -> isAlphaNum char && res) True str

readToken str
  | str == ";" = SEMICOLON
  | str == "->" = DERIVES
  | str == "|" = ALSODERIVES
  | str == "Episilon" || str == "EPSILON" || str == "epsilon" = EPSILON
  | (isAlphaNumString str) && (not $ null str) = SYMBOL str

grammarScan :: String -> ([Token], SymbolTable)
grammarScan str = undefined
    --words str



--IR, symbol table, list of non-terminals You can move the generation of the symbol table to here if you want.
grammarParse :: ([Token], SymbolTable) -> (IR, SymbolTable, [NonTerminal]) 
grammarParse = undefined
