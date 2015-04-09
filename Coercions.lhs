> module Coercions where

Basic coercion stuff

- finding.
- ? other utils.
- tests for coherence.

> import Data.List (partition)
> import Data.Maybe (catMaybes)

> import Debugging (trace, ftrace, show_term)
> import Base
> import Terms
> import Context

> import TermReduction(first_IVar, substitute)

> import Convertibility(convertible)
> import Convertibility(Substitution(..), emptySubstitution)
> import Convertibility(applySubstitution,isEmptySubstitution)

> import ConstraintTypeInf(findCoercion, CoInfo(..), WhichCoercion(..))

> import Reduction (dwhnf)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

TODO

reduce power - ie, to avoid IM. 

IS THIS MODULE REDUNDANT??????????????

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

> no_coercion = id :: Term -> Term

---
Try to find coerced term.

> find_coercion :: Ctxt -> TypedTerm -> Type -> String -> OkF (CoInfo, Term)
> find_coercion c tt@(TT arg ty_arg) tgt s
>  = case findCoercion c tt tgt of
>		Fail m   
>		   -> fail_with_msg "**** No Matching Coercions!"
>		Ok (ci, TT t ty)
>		   -> trace (unlines ["COERCIONS:", show ci, show t, show ty]) $
>		      return (ci, t)

---

WAS: local show fn, showing alternative paths...

> type Path = [(Name,Term,Type)]

> show_coercion_terms :: [Term] -> a -> a
> show_coercion_terms []
>  = trace ("No coercions\n")

<> show_coercion_terms fs
<>  = trace (unlines (map show_path fs) ++ "-----\n") 
<>    where
<>		show_path ss = "PATH:\n" ++ unlines (map show_step ss) 
<>		show_step [t] = "\t" ++ show t 



