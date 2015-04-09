> module UserSyntax where

for linking surface to internal and vice versa

#if __GLASGOW_HASKELL__ < 302
> import Trace
#else 
> import IOExts_(trace)
#endif

> -- import Maybe(isJust, fromJust)
> -- import Pretty 

> import Base

> import SimpleTerm
> import Terms
> import SharedSyntax

<> import TermOps(shiftUnderOneBinder, shiftOverOneBinder)

<> import PrintingAux

<> import ParsingAux(compileBrackets)		-- don't want this! 
<> import LF_Parser(SyntaxInfo)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
IMPORTANT PRINCIPLE

on the idea of prettyprinting back in to SpTT language.

when working in a SpTT, we don't want to see anything else.

we don't care about preservation of exact input form? 


%-------------------------------------------------------------------------------

REMEMBER TO INSERT EL to FO/DP types! and to match against them.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Precedence Tables

This lot can be included in Context? YES, for SI info.

%-------------------------------------------------------------------------------
TEMP

these to be part of the context, and to include the precedence tables.

might be useful, Name = Plain | Op (Oper | Backticks)
  - since Plain doesn't have precedence...


NB - the DP arrow must be done by SPECIAL CASE.
   - hence opt arrow should be HO fn, eg L -> R -> Term?
   - NO - can do it by BindVariety?

   - still, need extra to de-sugar the -> 

This means fiddling with imports? (ie, prefer no S_Term at ParsingAux)
  NB needs to be known, for the monad str. 

  - try to separate out the simple stuff? but without factoring below, am
    stuck.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

> type PrecTables = [(Name, Fixity)]
> type BracketTable = [(BracketPair, Maybe BracketArrow)]	
>                       -- ((start,end),arrow)

> parseTables :: BracketTable
> parseTables 
>  = [	(("{", "}"), Just "=>")
>		-- , (("<",">"), Just "#")
>		]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


