module LLGen where
import Reader

type FirstTable = [(Symbol, [Terminal])]
type FollowTable = [(NonTerminal, [Terminal])]
type NextTable = [(NonTerminal, [(Terminal,Int)])]

makeTables :: (IR, SymbolTable, [NonTerminal]) -> (FirstTable, FollowTable, NextTable)
makeTables = undefined


makeFirst :: IR - > SymbolTable -> FirstTable
makeFirst (terminals nonTerminals productions) symbolTable = 
    where first = map initFirst symbolList
          initFirst symbol = 
            if symbol `elem` nonTerminals then (symbol, [])
            else (symbol, [symbol])

makeFirstHelper (terminals nonTerminals productions) symbolTable first workList = 
    first' = map hel productions
    where helper (nt,symbols) = 
                where rhs = first $ head symbols
                      i = 1
    if first' == first then done
    else makeFirstHelper

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
