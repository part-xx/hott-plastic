> module Reduction (
>			whnf,
>			hnf, ihnf,
>			dwhnf, dwhnf_, Result(..),
>			dwhnf_to_ap,
>			etaContract,

>			reducibleApplication,
>			flatten,
>			unflatten,
>			applyToDummy_FlatTerm,
>			FlatTerm(..)

>			) where

#ifndef __GLASGOW_HASKELL__
#endif

> import Data.Maybe
> import Data.List((\\))

> import Base
> import Context
> import Terms
> import TermOps(isFirstIVarUsed, isIVarUsed)
> import TermReduction(substitute, shiftOverOneBinder, shiftUnderOneBinder)
> import TermReduction(first_IVar, db_start, flatten, flatten_aux)
> import TermReduction(compileArbitraryTerm)
> import SimpleTerm

> import Debugging (show_term, ftrace, ftracef, hack_show)
> import DebugOptions (traceOnDebugFlag)


%-------------------------------------------------------------------------------

> r_trace s
>  = traceOnDebugFlag "REDUCTION_TRACING" (s `fseq` s)

> r_tracef f r
>  = let s = f r in traceOnDebugFlag "REDUCTION_TRACING" (s `fseq` s) r

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

<WAS FLATTEN FN>

%-------------------------------------------------------------------------------
`applyToDummy_FlatTerm'
  - version of `applyToDummy' but working on terms in flat form.
  - it saves a bit of unflattening and re-flattening.

> applyToDummy_FlatTerm :: FlatTerm -> FlatTerm
> applyToDummy_FlatTerm (E t ts)
>  = E (shiftUnderOneBinder t) $ map shiftUnderOneBinder ts ++ [first_IVar]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Main Reduction Calls

	mix and match the possibilities added to basic whnf.

---
`headTerm' reduce to dwnhf and return the leftmost term.

> headTerm :: Term -> Term
> headTerm t
>  = case (dwhnf_ $ flatten t) of
>       E c _ -> c
>       A a   -> a

---
`dwhnf_to_ap'
  - like head Term, but intended for application situations only.

> dwhnf_to_ap :: Term -> OkF (Term, [Term])
> dwhnf_to_ap t 
>  = case dwhnf_ (flatten t) of
>		E c as -> Ok (c,as)
>		A _    -> Fail $ "dwhnf_to_ap: didn't reduce to application in " 
>							++ crude_print t

---
`whnf' has a simple implementation

> whnf :: Term -> Term
> whnf = unflatten . flatten

NB we ignore the eta. Maybe I need to rename wnhf to represent "just beta".
SAME COMMENT for dwhnf and hnf.

---
`dwhnf' reduces to whnf, expanding definitions where necessary.

so, goes to binder or var.

> dwhnf :: Term -> Term
> dwhnf t 
>  = r_tracef (\r -> "IN:  "++crude_print t++"\nOUT: "++crude_print r++"\n") $
>    unflatten $ dwhnf_ (flatten t)


---

> dwhnf_ :: FlatTerm -> FlatTerm
> dwhnf_ a@(A _) 
>  = a

** IMPORTANT **
QQ: why repeated def exp? surely should only do once? 

> dwhnf_ e@(E t ts)
>  = case delta_or_iota e of
>		Reduced d_or_e -> -- trace ("dwhnf D/I: " ++ crude_print t)
>		                  dwhnf_ d_or_e -- recurse? 
>		Blocked t      -> -- trace ("dwhnf stop: " ++ crude_print t)
>		                  t


---
`hnf' 
	- an abstr is reduced recursively.

> hnf :: Ctxt -> Term -> Term
> hnf c t
>  = r_trace ("hnf: " ++ crude_print t ++ "\n") $ 
>    h_ delta_or_iota c (flatten t)

---
`ihnf'
  - hnf but DON'T expand defs

> ihnf :: Ctxt -> Term -> Term
> ihnf c t
>  = r_tracef (\r -> "ihnf: " ++ show t ++ "\n    = " ++ show r ++ "\n") $ 
>    h_ shallow_iota c (flatten t)

---

> shallow_iota :: FlatTerm -> Result FlatTerm
> shallow_iota e 
>  = case iota e of
>		j@(Reduced _) -> j
>		Blocked t     -> case delta t of 
>					Nothing -> Blocked t
>					Just t' -> Reduced t'
>	-- still not sure about this.


---

> h_ :: (FlatTerm -> Result FlatTerm) -> Ctxt -> FlatTerm -> Term
> h_ rf c (A (DP v ty t)) = DP v (h_ rf c $ flatten ty) (h_ rf c $ flatten t)
> h_ rf c (A (FO v ty t)) = FO v (h_ rf c $ flatten ty) (h_ rf c $ flatten t)
> h_ rf c (A t) 
>  = error $ "Bad constructor in Reduction.A: " ++ show t

> h_ rf c e@(E (El t) ts) = El $ h_ rf c (flatten_aux ts t)

ABOVE CASE - ANOMALY - should decide what flatten does to kind-language.

> h_ rf c e@(E t ts)
>  = case rf e of
>		Blocked (E t ts) -> build_ap t $ map (h_ rf c . flatten) ts	-- recurse on children
>		Reduced e'       -> h_ rf c e'								-- recurse on reduced form



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Non-beta Reductions

These are the ones we do AFTER (whnf) beta stops.
They occur on FlatTerms, and return Maybe FlatTerms.
There are no errors that can occur in reductions, so Maybe is sufficient.


----
`reducibleApplication'
  - does next step of reduction for an application of a constant, if possible
  - for speed, some of the expansions are coded directly.
  - (ALTHOUGH THE COMPILER SHOULD DO THE INLINING etc! ie, I shouldn't worry)

AIM: replace this throughout ? 

> reducibleApplication :: FlatTerm -> Result FlatTerm

> -- definitions - CHECK EQUIVALENCE with delta def!
> reducibleApplication (E (GlobalDef _ rhs) ts) 
>  = Reduced $ flatten_aux ts rhs
> reducibleApplication (E (LocalDef  _ rhs) ts) 
>  = Reduced $ flatten_aux ts rhs
> reducibleApplication (E (Let v t1 t2)     ts) 
>  = Reduced $ flatten_aux ts (substitute t2 t1)

> -- otherwise, try iota
> reducibleApplication t 
>  = iota t


%-------------------------------------------------------------------------------
Delta.

> delta :: FlatTerm -> Maybe FlatTerm
> delta (E (GlobalDef d rhs) ts) = Just $ flatten_aux ts rhs
> delta (E (LocalDef  _ rhs) ts) = Just $ flatten_aux ts rhs
> delta (E (Let v t1 t2)     ts) = Just $ flatten_aux ts (substitute t2 t1) 
> delta _                        = Nothing


---
`delta_or_iota' 
  - try delta; if fails, try iota. 

> delta_or_iota :: FlatTerm -> Result FlatTerm
> delta_or_iota e 
>  = case delta e of
>		j@(Just t) -> Reduced t
>		Nothing    -> iota e





%-------------------------------------------------------------------------------
Iota - Computation Rule reduction

TODO:
 - speed up partition test? eg as a continuation? 
	 - NB 2-part test - for e_args and the constr. 
 - arity test for c_term? SHOULD BE OK SINCE IT ALL TYPE CHECKS! 

> r_traceON s = id -- ftrace s

> iota :: FlatTerm -> Result FlatTerm

- these cases should not occur 
- means simple_cvt isn't quite correct.
<> iota t@(E (Elim n a) []) 
<>  = Nothing		-- constant on its own, no args
<> iota t@(E (Ext (HardCast e@(Elim n a) _)) []) 
<>  = Nothing		-- constant on its own, no args

> iota t@(E (Ext (HardCast e@(Elim n a) _)) as) 
>  = r_trace ("IOTA CAST FOUND: " ++ show n ++ "\n") $ 
>    iota (E e $ unCAST_specific a as)

DODGY CASE. 
removes the cast for the constr, and stops it being hit by the special case 
below.

> iota t@(E eop@(Elim n arity) as)
>  | arity /= length as 
>     = Blocked t
>  | otherwise 
>  = _scc_ "iota:Elim"
>    r_traceON ("IOTA "++show n++"("++show arity++")  "++show constr ++ "\n")
>  $ case (dwhnf_ $ flatten constr) of
>	  E (Const n f) c_args
>		-> _scc_ "iota:Elim:CONST" 
>	           Reduced $ applyIotaFn f (c_args ++ fast_r_args)
>	  E (F_Const n f) c_args
>		-> _scc_ "iota:Elim:F_Const" 
>	           Reduced $ f (reverse c_args ++ fast_r_args)

HERE.

>	  e@(E (Ext (HardCast (Const n f) _)) c_args)
>		-> r_traceON ("IOTA=" ++ unlines [show n, show_term emptyCtxt f, unlines $ map (show_term emptyCtxt) c_args]) $ 
>		   Reduced $ applyIotaFn f (c_args ++ fast_r_args)
>	  e@(E (Ext (HardCast (F_Const n f) _)) c_args)
>		-> Reduced $ f (reverse c_args ++ fast_r_args)

>
>	  e@(E t _) -> trace_non_const (e,t) 
>	             $ Blocked $ E eop (init as ++ [unflatten e])

>	  a@(A _)   -> error $ "constr went to: " ++ show a

>   where
>	(constr:r_args) = reverse as
>	fast_r_args = _scc_ "fast_r_args"
>	              map compileArbitraryTerm r_args
>
>	trace_non_const et = id
>	-- trace_non_const et = ftrace ("FOUND: " ++ take 200 (show et) ++ "\n")


> iota t@(E eop@(F_Elim n) as)
>  = _scc_ "iota:F_Elim"
>    r_traceON ("F_IOTA "++show n++" "++unlines (map show as) ++ "\n")
>  $ case (dwhnf_ $ flatten constr) of
>	E (Const n f) c_args
>		-> _scc_ "iota:F_Elim:CONST" 
>		   Reduced $ applyIotaFn f (reverse c_args ++ r_args)

>	E (F_Const n f) c_args
>		-> _scc_ "iota:F_Elim:F_Const" 
>		   Reduced $ f (reverse c_args ++ r_args)

>	E (Closed t _) c_args
>		-> error $ "Closed Term, iota F_Elim = " ++ show t ++ "\n"
>						++ unlines (map show as) 

>	e@(E t _) -> trace_non_const (e,t) 
>	           $ Blocked $ E eop (init as ++ [unflatten e])

>	a@(A _)   -> error $ "(F)constr went to: " ++ show a
>    where
>		-- (constr:r_args) = as
>		(constr:r_args) = reverse as
>		trace_non_const et = id
>		-- trace_non_const et = ftrace ("FOUND: " ++ take 200 (show et) ++ "\n")

#if 0
>						ftrace ("C" ++ show a ++ " " ++ n ++ "\n" ) $ 
>						ftrace ("RHS = " ++ hack_show f ++ "\n") $ 
>						ftrace ("Args =\n" ++ unlines (map hack_show $ 
>												e_args ++ c_args ++ tail_as)) $ 
#endif

> iota t = Blocked t

---
`applyIotaFn'
  - expects args in REVERSE order

> applyIotaFn :: Term -> [Term] -> FlatTerm
> applyIotaFn f as = flatten_aux as f


---
`unCAST_specific'
  - uncast the CONSTRUCTOR expression
  - do it carefully - we don't want to undo too much.
  - NB I am unsure about which cases really are required - hence the caution

> unCAST_specific :: Arity -> [Term] -> [Term]
> unCAST_specific 1 (a:as)
>  = case flatten a of
>		E (Ext (HardCast c@(Const _ _) _)) cas   -> build_ap c cas : as
>		E t ts                                   -> build_ap t ts  : as

<> unCAST_specific 1 ts                     = ts
	- WAS case: anything else is ok

> unCAST_specific n ts 
>  = error $ "unCAST_specific new case = " ++ show (n,ts)



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
New mechanism

> data Result a
>  = Blocked a | Reduced a








%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
FOR OLD ETA CODE - see RCS files.

`etaContract'
  - simple op. on term
  - ([x:A]f x) -> f, unless x occurs in f.
  - but tries to strip off as much as possible, 
  - eg ([x,y:A]f y y x) -> [y:A]f y y
  - IMPORTANT: this isn't a general function - only for lambda-terms
    eg it doesn't do full traversal of Term type.

> etaContract :: Term -> Term
> etaContract 
>  = either (\(bvs,t) -> foldr (\(v,ty) -> FO v ty) t $ reverse bvs) id  
>  . er []
>    -- pile unused bindings back on the term, in case of blocked reduction

> er bvs ft@(FO v ty t)                         
>  = er ((v,ty) : bvs) t 
>    -- pop binder on stack, to later get rid of (see how taken off list below)

> er []  t = Right t
>   -- no more binders to peel off, return ok.

> er bvs@(_:bvs_tail) at@(Ap t iv@(IVar j)) 
>  | j == db_start && not (isFirstIVarUsed t)
>  = er bvs_tail (shiftOverOneBinder t)
>   -- only continue with 'rator' if the 'rand' is sgl use of IVar 0.
>   -- the popped binder is the most recent encountered

<> er bvs (Co l r) 
<>  = er bvs (Ap l r)
<>    -- make coercion case act like application.
<>    -- PROVISIONAL - is this what we want? 
<>    -- DISABLED - caller now has to manage this detail

> er bvs t = Left (bvs,t)
>   -- no binder or app-to-sgl-var, so fail with list of binders waiting

PROBLEM: working with the coercion case?  Eg (co (cx3 A) b)

<> -- test values
<> tr3 = FO (Bound $ Plain "x") Type tr2
<> tr2 = FO (Bound $ Plain "y") Type tr1
<> tr1 = FO (Bound $ Plain "z") Type 
<>     $ Ap (Ap (Ap (MetaVar (Plain "f")) (IVar 1)) (IVar 1)) (IVar 0)
<> idx = FO (Bound $ Plain "z") Type $ Ap (MetaVar (Plain "f")) (IVar 0)
<> trB = FO (Bound $ Plain "x") Type $ FO (Bound $ Plain "y") Type $ Ap (Ap (MetaVar (Plain "f")) (IVar 0)) (IVar 1)
<> trC = FO (Bound $ Plain "x") Type $ FO (Bound $ Plain "y") Type $ Ap (Ap (MetaVar (Plain "f")) (IVar 1)) (IVar 0)
<> trC = FO (Bound $ Plain "x") Type $ Ap (FO (Bound $ Plain "y") Type $ Ap (MetaVar (Plain "f")) (IVar 0)) (IVar 0)


