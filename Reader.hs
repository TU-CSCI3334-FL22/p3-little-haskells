module Reader where
import Data.Char
import Data.List

type Terminal = String
type NonTerminal = String
type Symbol = String
type SymbolTable = [Symbol] --Map of strings to ints 
data Token = SEMICOLON | DERIVES | ALSODERIVES | EPSILON | SYMBOL String deriving (Show, Eq)
type Production = (NonTerminal, [Symbol])
data IR = IR [Terminal] [NonTerminal] [(Integer, Production)] deriving (Show)

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
          symbolTable  = nub $ map symbolString symbolToks


stringFromSymbol (SYMBOL str) = str


--IR, symbol table, list of non-terminals You can move the generation of the symbol table to here if you want.
grammarParse :: ([Token], SymbolTable) -> (IR, SymbolTable, [NonTerminal]) 
grammarParse (tokenList, symbolTable) = (IR terminals nonTerminals productionz, symbolTable, nonTerminals)
    where (productions, nonTerminals) = buildProductionList tokenList
          terminals = filter (\x -> not (elem x nonTerminals)) symbolTable
          productionz = zip [1..] productions 

buildProductionList :: [Token] -> ([Production],[NonTerminal])
buildProductionList tokens =  (productionSet++productionList,nt:nonTerminals)
  where (productionSet,nt,tokens') = buildProductionSet tokens
        helper (SEMICOLON:tokens2) = buildProductionList' tokens2
        helper _ = error "Invalid production list"
        (productionList,nonTerminals, tokens'') = helper tokens'


buildProductionList' :: [Token] -> ([Production],[NonTerminal],[Token])
buildProductionList' [] = ([],[],[])
buildProductionList' tokens@(SYMBOL _:_) =  (productionSet++productionList,nt:nonTerminals,tokens'')
  where (productionSet,nt,tokens') = buildProductionSet tokens
        helper (SEMICOLON:tokens2) = buildProductionList' tokens2 
        helper _ = error "Invalid production list'"
        (productionList, nonTerminals, tokens'') = helper tokens'
buildProductionList' toks = error $ show toks

buildProductionSet :: [Token] -> ([Production],NonTerminal,[Token])
buildProductionSet ((SYMBOL s):DERIVES:tokens) = (production:productionSet', s, tokens'')
  where (rhs, tokens') = buildRightHandSide tokens
        (productionSet', tokens'') = buildProductionSet' tokens' s
        production = (s,rhs)
buildProductionSet tokens  = error $ "Invalid production set on token: " ++ show (tokens)

buildProductionSet' :: [Token] -> NonTerminal -> ([Production],[Token])
buildProductionSet' (ALSODERIVES:tokens) nt = (production:productionSet', tokens'')
  where (rhs, tokens') = buildRightHandSide tokens
        (productionSet', tokens'') = buildProductionSet' tokens' nt
        production = (nt,rhs)
buildProductionSet' (SEMICOLON:tokens) nt = ([],SEMICOLON:tokens)              
buildProductionSet' tokens nt = error $ "Invalid production set'" ++ show (tokens) ++ show nt

buildRightHandSide :: [Token] -> ([Symbol],[Token])
buildRightHandSide (EPSILON:tokens) = ([""],tokens)
buildRightHandSide tokens = buildSymbolList tokens

buildSymbolList' :: [Token] -> ([Symbol],[Token])
buildSymbolList' ((SYMBOL s):tokens) = (s:symbolList,tokens')
  where (symbolList,tokens') = buildSymbolList' tokens
buildSymbolList' (ALSODERIVES:tokens) = ([],ALSODERIVES:tokens)
buildSymbolList' (SEMICOLON:tokens) = ([],SEMICOLON:tokens)
buildSymbolList' _ = error "Invalid symbol list'"

buildSymbolList :: [Token] -> ([Symbol],[Token])
buildSymbolList ((SYMBOL s):tokens) = (s:symbolList,tokens')
  where (symbolList,tokens') = buildSymbolList' tokens
buildSymbolList _ = error "Invalid symbol list"


--[SYMBOL "abc", SEMICOLON]


