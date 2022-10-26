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
          first = map initFirst symbols
          first' = makeFirstHelper productions first

lookupFirst :: Symbol -> FirstTable -> [Terminal]
lookupFirst "" _ = [""]
lookupFirst "<EOF>" _ = ["<EOF>"]
lookupFirst elem firstTable = fromJust $ lookup elem firstTable

makeFirstHelper :: [(Integer,Production)] -> FirstTable -> FirstTable
makeFirstHelper productions first = 
    let updateProduction (n,(a,bs)) = (a,firstOfA')
            where firstOfB1 = lookupFirst (head bs) first
                  rhs =  filter (not . null) firstOfB1
                  helper (b:bs) rhs i = 
                    let firstOfBi = lookupFirst b first in
                    if (elem "" firstOfBi) && (not $ null bs)
                    then helper bs (nub $ rhs ++ (filter (not . null) firstOfBi)) (i+1)
                    else (rhs, i)
                  (rhs', i) = helper bs rhs 1
                  rhs'' = if (i == length bs) && (elem "" $ lookupFirst (last bs) first)
                         then "":rhs'
                         else rhs' 
                  firstOfA = lookupFirst a first
                  firstOfA' = nub $ firstOfA ++ rhs''
        first' = map updateProduction productions
    in  if first' == first 
        then first'
        else makeFirstHelper productions first'

--todo: use worklist 

--(ir, symbolTable, nts) = grammarParse $ grammarScan ""

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
