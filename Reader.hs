module Reader where
import Data.Char
import Data.List.Split

type Terminal = String
type NonTerminal = String
type Symbol = String
type SymbolTable = [Symbol] --Map of strings to ints 
data Token = SEMICOLON | DERIVES | ALSODERIVES | EPSILON | SYMBOL String deriving (Show, Eq)
type Production = (NonTerminal, [Symbol])
data IR = IR [Terminal] [NonTerminal] [(Integer, Production)]

isAlphaNumString str = foldl (\res char -> isAlphaNum char && res) True str

readToken str
  | str == ";" = SEMICOLON
  | str == ":" = DERIVES
  | str == "|" = ALSODERIVES
  | str == "Episilon" || str == "EPSILON" || str == "epsilon" = EPSILON
  | (isAlphaNumString str) && (not $ null str) = SYMBOL str

isSYMBOL (SYMBOL str) = True
isSYMBOL _ = False

symbolString (SYMBOL str) = str

grammarScan :: String -> ([Token], SymbolTable)
grammarScan str = (tokenList, symbolTable)
    where tokenList = map readToken (words str)
          symbolToks = filter isSYMBOL tokenList
          symbolTable  = map symbolString symbolToks


stringFromSymbol (SYMBOL str) = str


--IR, symbol table, list of non-terminals You can move the generation of the symbol table to here if you want.
grammarParse :: ([Token], SymbolTable) -> (IR, SymbolTable, [NonTerminal]) 
grammarParse (tokenList, symbolTable) = (IR terminals nonTerminals productions, symbolTable, nonTerminals)
    where productions = buildProductionList tokenList



buildProductionList :: [Token] -> ([Production],[Token])
buildProductionList tokens =  (productionSet++productionList,tokens'')
  where (productionSet,tokens') = buildProductionSet tokens
        helper (SEMICOLON:tokens2) = buildProductionList' tokens2
        helper _ = error "invalid production list"
        (productionList, tokens'') = helper tokens'

buildProductionList' :: [Token] -> ([Production],[Token])
buildProductionList' tokens =  (productionSet++productionList,tokens'')
  where (productionSet,tokens') = buildProductionSet tokens
        helper (SEMICOLON:tokens2) = buildProductionList' tokens2
        helper _ = error "invalid production list'"
        (productionList, tokens'') = helper tokens'
buildProductionList' (EPSILON:tokens) = ([],tokens)
buildProductionList' _ = error "Invalid production list'"

buildProductionSet :: [Token] -> ([Production],[Token])
buildProductionSet ((SYMBOL s):DERIVES:tokens) = (production:productionSet', tokens'')
  where (rhs, tokens') = buildRightHandSide tokens
        (productionSet', tokens'') = buildProductionSet' tokens' s
        production = (s,rhs)

buildProductionSet' :: [Token] -> NonTerminal -> ([Production],[Token])
buildProductionSet' (ALSODERIVES:tokens) nt = (production:productionSet', tokens'')
  where (rhs, tokens') = buildRightHandSide tokens
        (productionSet', tokens'') = buildProductionSet' tokens' nt
        production = (nt,rhs)
buildProductionSet'                  
buildProductionSet' _  = error "Invalid production set'"

buildRightHandSide :: [Token] -> ([Symbol],[Token])
buildRightHandSide (EPSILON:tokens) = ([],tokens)
buildRightHandSide tokens = buildSymbolList tokens

buildSymbolList' :: [Token] -> ([Symbol],[Token])
buildSymbolList' ((SYMBOL s):tokens) = (s:symbolList,tokens')
  where (symbolList,tokens') = buildSymbolList' tokens
buildSymbolList' (ALSODERIVES:tokens) = ([],tokens)
buildSymbolList' (SEMICOLON:tokens) = ([],tokens)
buildSymbolList' _ = error "Invalid symbol list'"

buildSymbolList :: [Token] -> ([Symbol],[Token])
buildSymbolList ((SYMBOL s):tokens) = (s:symbolList,tokens')
  where (symbolList,tokens') = buildSymbolList' tokens
buildSymbolList _ = error "Invalid symbol list"


--[SYMBOL "abc", SEMICOLON]


