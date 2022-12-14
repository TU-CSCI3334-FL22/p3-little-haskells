module LLGen where
import Reader
import Data.List
import Data.Maybe
import Debug.Trace

type FirstTable = [(Symbol, [Terminal])]
type FollowTable = [(NonTerminal, [Terminal])]
type NextTable = [(NonTerminal, [(Terminal,Integer)])]

makeTables :: (IR, SymbolTable, [NonTerminal]) -> (FirstTable, FollowTable, NextTable)
makeTables (ir, symbols, nt) = (first, follow, next)
  where 
      first = makeFirst ir symbols
      follow = makeFollow ir symbols first
      next = makeNext ir first follow 


makeFirst :: IR -> SymbolTable -> FirstTable
makeFirst (IR terminals nonTerminals productions) symbols = first'
    where initFirst symbol = 
            if symbol `elem` nonTerminals then (symbol, [])
            else (symbol, [symbol])
          first = (map initFirst symbols) ++ [("",[""]),("<EOF>",["<EOF>"])]
          first' = makeFirstHelper productions first

--appendToTable :: Symbol -> [Symbol] -> [(Symbol,[Symbol])] -> [(Symbol,[Symbol])] 
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
next = makeNext ir first follow
putStrLn $ showTables (first,follow,next)
putStr $ showNextYaml next
putStr $ toYaml ir next

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

--[(NonTerminal, [(Terminal,Int)])]
makeNext:: IR -> FirstTable -> FollowTable -> NextTable
makeNext (IR terminals nonTerminals productionz) first follow = 
  let next = map (\nt -> (nt, [])) nonTerminals
      collectSymbs (n,(a,[])) = (fromJust $ lookup a follow, True)
      collectSymbs (n,(a,b:bs)) =
        let firstB = fromJust $ lookup b first 
                in if elem "" firstB
           then let (next', hasEps) = collectSymbs (n, (a,bs))
                in (nub $ firstB ++ next', hasEps)
           else (firstB, False)
      updateNext next (n,(a,b)) = 
        let (symbs,hasEps) = collectSymbs (n,(a,b))
            symbs' = if hasEps then symbs else filter (not . null) symbs
        in appendToTable a (map (\s -> (s,n)) symbs') next
  in foldl (\res p -> updateNext res p) next productionz
  

showTables ::  (FirstTable, FollowTable, NextTable) -> String
showTables (first,follow,next) = "FIRST TABLE \n" ++ showFirst first ++ "\nFOLLOW TABLE\n" ++ showFollow follow ++"\nNEXT TABLE\n"++ showNext next 

showFirst :: FirstTable -> String 
showFirst first = unlines $ map (\(symb,lst) -> (take 6 symb) ++ "\t| " ++ show lst) first  
 
showFollow :: FollowTable -> String 
showFollow follow = unlines $ map (\(symb,lst) -> (take 6 symb) ++ "\t| " ++ show lst) follow

--type NextTable = [(NonTerminal, [(Terminal,Integer)])]
showNext :: NextTable -> String
showNext next = header ++ "\n" ++ (unlines $ map (\(nt,row) -> (take 6 nt) ++ "\t| " ++ showRow row) next)
  where terminals = nub $ foldl (\terms (nt,row) -> terms ++ (map fst row)) [] next
        showEntry (Just i) = show i ++ "\t|"
        showEntry Nothing = "--\t|"
        header = "|\t|" ++ (concat $ map (\t -> (take 6 t) ++ "\t|") terminals)
        showRow row = concat $ map (\t -> showEntry $ lookup t row) terminals

toYaml ::  IR -> NextTable -> String
toYaml (IR terminals nonTerminals productionz) next = line1 ++ line2 ++ line3 ++ line4 ++ line5 ++ line6 ++ line7
  where line1 = "terminals: " ++ (printHelper $ show $ remeps terminals) ++ "\n"
        line2 = "non-terminals: " ++ (printHelper $ show nonTerminals) ++ "\n"
        line3 = "eof-marker: <EOF> \n"
        line4 = "error-marker: -- \n" 
        line5 = "start-symbol: " ++ head nonTerminals ++ "\n"
        line6 = "productions: \n" ++ (concat $ map(\(n,(nt, p)) -> "  " ++ (show n) ++ ": {" ++ nt ++ ": " ++ (printHelper $ show p) ++ "}\n") productionz)
        line7 = "table:\n" ++ showNextYaml next
fixLL :: (IR, SymbolTable, [NonTerminal])  -> (IR, SymbolTable, [NonTerminal]) 
fixLL = undefined

remeps = filter (/= "EPSILON")
printHelper :: String -> String
printHelper str = filter(\n -> n /= '\"') str
uneps "" = "EPSILON"
uneps x = x

showNextYaml :: NextTable -> String
showNextYaml next =  unlines $ map (\(nt,row) -> "  " ++ nt ++ ": {" ++ (init $ init $ showRow row) ++ "}") next
  where terminals = nub $ foldl (\terms (nt,row) -> terms ++ (map fst row)) [] next
        showEntry (Just i) = show i ++ ", "
        showEntry Nothing = "--, "
        showRow row = concat $ map (\t -> (uneps t) ++ ": " ++ (showEntry $ lookup t row)) terminals
        --showRow row = concat $ map(\(t,i) -> t ++ ": ")
