| | | | |
| --------|-------|---| ---|
| 1 | Grammar | $\rightarrow$ | ProductionList |
|    2 | ProductionList | $\rightarrow$ |  ProductionSet SEMICOLON ProductionList'|
|    3 | ProductionList' | $\rightarrow$| ProductionSet SEMICOLON ProductionList' |
|    4 |                | $\mid$ | EPSILON|
|    5 | ProductionSet | $\rightarrow$ |  SYMBOL  DERIVES  RightHandSide ProductionSet'|
|    6 | ProductionSet'| $\rightarrow$ | ALSODERIVES RightHandSide ProductionSet'| 
|    7 |               | $\mid$ | EPSILON
|    8 | RightHandSide | $\rightarrow$ |  SymbolList|
|    9 |               | $\mid$ | EPSILON|
|    10 | SymbolList | $\rightarrow$ |  SYMBOL SymbolList'|
|    11 | SymbolList'| $\rightarrow$ | SYMBOL SymbolList'| 
|    12| | $\mid$ | EPSILON|
        
