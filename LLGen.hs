module LLGen where
import Reader
import Data.List
import Data.Maybe

type FirstTable = [(Symbol, [Terminal])]
type FollowTable = [(NonTerminal, [Terminal])]
type NextTable = [(NonTerminal, [(Terminal,Int)])]

makeTables :: (IR, SymbolTable, [NonTerminal]) -> (FirstTable, FollowTable, NextTable)
makeTables = undefined


makeFirst :: IR -> SymbolTable -> FirstTable
makeFirst (IR terminals nonTerminals productions) symbols = first'
    where initFirst symbol = 
            if symbol `elem` nonTerminals then (symbol, [])
            else (symbol, [symbol])
          first = (map initFirst symbols) ++ [("",[""]),("<EOF>",["<EOF>"])]
          first' = makeFirstHelper productions first

replaceFirst a firstOfA firstTable = 
  map (\(x,firstOfX) -> if x == a then (a,firstOfA) else (x,firstOfX)) firstTable

makeFirstHelper :: [(Integer,Production)] -> FirstTable -> FirstTable
makeFirstHelper productions first = 
    let updateProduction (a,bs) = firstOfA'
            where firstOfB1 = fromJust $ lookup (head bs) first
                  rhs =  filter (not . null) firstOfB1
                  helper (b:bs) rhs i = 
                    let firstOfBi = fromJust $ lookup b first in
                    if (elem "" firstOfBi) && (not $ null bs)
                    then helper bs (nub $ rhs ++ (filter (not . null) firstOfBi)) (i+1)
                    else (rhs, i)
                  helper [] rhs i = error $ " empty production " ++ a
                  (rhs', i) = helper bs rhs 1
                  rhs'' = if (i == length bs) && (elem "" $ fromJust $ lookup (last bs) first)
                         then "":rhs'
                         else rhs' 
                  firstOfA = fromJust $ lookup a first
                  firstOfA' = nub $ firstOfA ++ rhs''
        first' = foldl (\f (n,(a,bs)) -> replaceFirst a (updateProduction (a,bs)) f) first productions
    in  if first' == first 
        then first'
        else makeFirstHelper productions first'

--todo: use worklist 

{-
(ir, symbolTable, nts) = grammarParse $ grammarScan "Goal   : Expr         ;  Expr   : Term EPrime         ;  EPrime : PLUS  Term EPrime        | MINUS Term EPrime        | epsilon         ;  Term   : Factor TPrime         ;  TPrime : TIMES Factor TPrime        |  DIV   Factor TPrime        | epsilon         ;  Factor : LP Expr RP        | NUMBER        | IDENTIFIER        ;  "
makeFirst ir symbolTable
-}

makeFollow:: (IR, SymbolTable, [NonTerminal]) -> FollowTable
makeFollow = undefined

makeNext:: (IR, SymbolTable, [NonTerminal]) -> NextTable
makeNext = undefined 

showTables ::  (FirstTable, FollowTable, NextTable) -> String
showTables = undefined

toYaml ::  (FirstTable, FollowTable, NextTable) -> String
toYaml = undefined

fixLL :: (IR, SymbolTable, [NonTerminal])  -> (IR, SymbolTable, [NonTerminal]) 
fixLL = undefined
