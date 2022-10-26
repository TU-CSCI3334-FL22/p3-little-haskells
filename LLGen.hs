module LLGen where
import Reader
import Data.List
import Data.Maybe
import Debug.Trace

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

appendToTable :: Symbol -> [Symbol] -> [(Symbol,[Symbol])] -> [(Symbol,[Symbol])] 
appendToTable a tableOfA table = 
  map (\(x,tableOfX) -> if x == a then (a,nub $ tableOfX ++ tableOfA) else (x,tableOfX)) table

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
        first' = foldl (\f (n,(a,bs)) -> appendToTable a (updateProduction (a,bs)) f) first productions
    in  if first' == first 
        then first'
        else makeFirstHelper productions first'

--todo: use worklist 

{-
(ir, symbolTable, nts) = grammarParse $ grammarScan "Goal   : Expr         ;  Expr   : Term EPrime         ;  EPrime : PLUS  Term EPrime        | MINUS Term EPrime        | epsilon         ;  Term   : Factor TPrime         ;  TPrime : TIMES Factor TPrime        |  DIV   Factor TPrime        | epsilon         ;  Factor : LP Expr RP        | NUMBER        | IDENTIFIER        ;  "
first = makeFirst ir symbolTable
follow = makeFollow ir symbolTable first
-}

makeFollow:: IR -> SymbolTable -> FirstTable -> FollowTable
makeFollow (IR terminals nonTerminals productions) symbols first = follow'
    where follow = (head nonTerminals, ["<EOF>"]):(map (\nt -> (nt,[])) (tail nonTerminals))
          follow' = makeFollowHelper productions nonTerminals first follow

appendWithoutEpsilon a b = 
  nub $ filter (not . null) (a ++ b)

makeFollowHelper :: [(Integer,Production)] -> [NonTerminal] -> FirstTable -> FollowTable -> FollowTable
makeFollowHelper productions nonTerminals first followTable = 
    let updateProduction (a,bs) follow = follow'
            where helper [] trailer f = (f,trailer)
                  helper symbs trailer f = 
                    let b = last symbs
                        firstB = fromJust $ lookup b first
                        (f',trailer') = 
                          if b `elem` nonTerminals
                          then if elem "" firstB
                              then (appendToTable b trailer f, appendWithoutEpsilon trailer firstB)
                              else (appendToTable b trailer f, firstB)
                          else (f, fromJust $ lookup b first)
                    in helper (init symbs) trailer' f'
                  (follow',trailer) = helper bs (fromJust $ lookup a follow) follow
        followTable' = foldl (\f (n,(a,bs)) -> updateProduction (a,bs) f) followTable productions
    in  if followTable' == followTable 
        then followTable'
        else makeFollowHelper productions nonTerminals first followTable'


makeNext:: (IR, SymbolTable, [NonTerminal]) -> NextTable
makeNext = undefined 

showTables ::  (FirstTable, FollowTable, NextTable) -> String
showTables = undefined

toYaml ::  (FirstTable, FollowTable, NextTable) -> String
toYaml = undefined

fixLL :: (IR, SymbolTable, [NonTerminal])  -> (IR, SymbolTable, [NonTerminal]) 
fixLL = undefined
