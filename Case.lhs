> module Case where

> import Base

> import CommandTypes
> import SimpleTerm

<> import Terms

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

This has to call SimpleToTerm?

Then STT might have to call translateCase...

translateCase could be shifted to a lower level? 

%---------------------------------------

AIM to be clear in this section

* is (S~T~ -> S~T~) the correct way to go? 
  (ok perhaps if we definitely want to write as elim...)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
STAGE ONE.

convert general defs in to equivalent case expr.

introduce fixpt markings.

Q: how to handle local defs? lift up to where they are bound, and make in to
Local Let?


> processFunctionDef d@(FnDefinition cs ls)
>  = error $ "NYI:\n" ++ show d
>    --- translateCase ....


<> data FunctionDefinition = FnDefinition [FunctionClause] LocalFunctionDefs
<> type LocalFunctionDefs  = Maybe [FunctionDefinition]
<> data FunctionClause     = FnClause Name [SimpleTerm] SimpleTerm 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

impl: purely an operation on simple-term format.

<> translateCase 
<>  :: Context -> [SimpleTerm] -> [([SimpleTerm], SimpleTerm)] -> m? SimpleTerm

> translateCase c vs as
>  = error $ "NYI tC:\n" ++ show vs ++ "\n" ++ unlines (map show as)

1. check expr-pattern count matches.
2. split in to nested single case, complete with shared lets for impl of _
   NB the lets might contain recursive calls...

3. lift out recursive function calls - adt to label what is happening.
	- eg data Foo = Plain | UsesParam | UsesCall
	- 

4. type inference - find non-recursive? 

3. then the magic bit(!)


