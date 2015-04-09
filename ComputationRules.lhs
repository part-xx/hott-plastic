> module ComputationRules (
>			addElimRules,
>			makeElimCyclic,
>			simple_compileElimRules
>		) where


> -- import Maybe
> import Control.Monad(foldM)

> import Base
> import Terms
> import TermOps(replaceTerminal)
> import TermReduction(compileReductionAtom)
> import Context
> import ContextOps

> import CommandTypes
> import Printing

> import Debugging(show_term)
> import DebugOptions(traceOnDebugFlag)
> import GlobalOptions(p_Normal)

> import SimpleTerm
> import SimpleToTerm(translate_db)

> import TypeInference (typeCheckRawKind, TypedTerm(..))



%-------------------------------------------------------------------------------
Switchable, forced trace.

> cr_trace m = traceOnDebugFlag "COMPRULES_TRACING" (m `fseq` m)

> cr_trace_m m = cr_trace m $ return ()


%-------------------------------------------------------------------------------

> type E_Arity = Arity
> type E_Name = Name
> type C_Name = Name
> type C_Arg  = SimpleTerm


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Adding Computation Rules for Iota Reduction.

For a given E, if rules have form 
\begin{verbatim}
	(E a1 .. am, c(x1..xn)) = f(a1...am, x1..xn) : K...
\end{verbatim}
then make E an Elim operator and modify the constrs to work with E.

There are two ways such rules can be generated:
	* via the inductive family schemata.
	* by hand.


Two stages: MOVE.
	1. Compile the comp rule syntax to conventional functions
	2. Check that the result is well-typed and add it to the context.
	3. Adjust... 

TECHNIQUE USED:
	When an Elim operator application is saturated, it may be reduced.
	The first (n-1) arguments of the elim operator (ie, without the 
	constructor) plus the m arguments of the constructor are passed to
	the function which is stored (internally) in the constructor term.

	This function then should apply one of the arguments to some of the other
	arguments, or call the Elim operator recursively.

CYCLIC TERMS:
	The above introduces a dependence between context entries for E_X and 
	the constructors being declared.

	Eg, see the comp. rule for Lambda (LL) in Sol.

	In particular, the TYPE of E_X may contain constructors of the inductive
	family X (and hence contain the function of the constructor) 

	Constructor functions may refer to themselves AS A TYPE.

	Any change eg discharge must repair the dependency.
	makeElimCyclic - does the main one? 

	NB don't need the cyclicity when doing a type check of the constrs.



NON-LINEAR PATTERNS (1) - Inductive Families
	There is NO SPECIAL TREATMENT for these, since we conjecture that the
	type restrictions on a function produced from a constructor signature 
	are such that convertibility of the arguments is guaranteed.

NON-LINEAR PATTERNS (2) - Manually Generated Rules
	NOT IMPLEMENTED. 
	
	We might need to add conv tests AFTER translation. 
	Might want to specify as bindings + rhs + something_else_for_ nonlin pat.

	This might be sensible - NB transl could be simple. and we could then
		GENERALISE the compilation process.



COERCIONS and EL-INSERTION
	difficult at the moment, because cyclicity inhibits type inference

	make-recursive at last step? XXXX

%---------------------------------------
Simplified version
	- full version should do a lot more CHECKING! 
	- and should allow nonlinear patterns? (NB check for FA_Elim?)


> simple_compileElimRules :: Name -> Name -> Arity -> [CompRuleFunction] -> IM
> simple_compileElimRules it_name eo_name eo_arity rs
>  = do 
>		addElimRules translate_db eo_name eo_arity rs
>		chg_M_ (chgCtxt it_name chg_ce)
>    where
>	constrs = [ n | CRF n _ _ _ <- rs ]
>	chg_ce (CE (Hyp a) ty) = Ok $ CE (IndFamily constrs a fake) ty
>	chg_ce (CE _ _)        = Fail $ "CE not Hyp for Ind Family name: " 
>							++ show it_name
>	fake = "non-standard" ++ error " shouldn't reach"

VERY TEMPORARY. 


%---------------------------------------
Second Stage: 

SEE REMARKS ABOUT WHEN TO ADD CONV TESTS! 

TROUBLE: need to delay transl of rules until ctxt stuff added.


> type TranslateFn = Ctxt -> SimpleTerm -> OkF Term

> addElimRules :: TranslateFn -> Name -> Arity -> [CompRuleFunction] -> IM
> addElimRules transl e_name e_arity crs
>  = catch_M_ fail_with_msg $ 
>    do
>		chg_M_ (chgCtxt e_name mk_e)
>		chg_M_ (rewritePostContext (replaceTerminal chg_to_elim_op) e_name)
>		ok <- mapM (check_and_add_comp_rule e_name transl) crs
>		makeElimCyclic transl e_name crs
>		return ()
>    where
>		e_term = Elim e_name e_arity

>		mk_e (CE (Hyp _) ty) = Ok $ CE (ElimOp e_arity e_term) ty
>		mk_e (CE _ _)        = Fail $ "CE not Hyp for Elim Op: " ++ show e_name

>		chg_to_elim_op (ContextVar n) | n == e_name = Just e_term
>		chg_to_elim_op _                            = Nothing

NOTE (sep2000) - need to update later occs of E name. 


---
`makeElimCyclic' 
  - change occurrences of the constructor names in the elim rule's type
	to their `compiled' versions.
  - obtains the compiled versions by translating the name through the 
	translation function. (this could be done directly, after chg of abstr).

> makeElimCyclic :: TranslateFn -> Name -> [CompRuleFunction] -> IM
> makeElimCyclic transl e_name crs
>  = do
>		let c_names = [ n | CRF n _ _ _ <- crs ]
>		c <- get_M_

>		c1 <- embed $ foldM (\c n -> mk_cycle transl e_name n c) c c_names

>		set_M_ c1

>		-- print_M_ "Cyclic stuff done\n"


---


> mk_cycle :: TranslateFn -> Name -> Name -> Ctxt -> OkF Ctxt
> mk_cycle transl e_name c_name c_0
>  = do
>			-- get the current version of e.
>		e_term <- get_compiled e_name c_0

>			-- replace e_term in c_def
>			-- e type doesn't matter in the c_term.
>		c_1 <- chgCtxt c_name (chg_c $ replace e_name e_term) c_0

>			-- now replace in e (type) the NEW value of c_name
>		c_term <- get_compiled c_name c_1
>		c_2 <- chgCtxt e_name (chg_e $ replace c_name c_term) c_1

>		return c_2
>    where
>		get_compiled n c = transl c (Name_S n)

>		chg_e chg_f (CE e@(ElimOp _ _) ty)
>		 = Ok $ CE e $ chg_f ty
>		chg_e chg_f (CE _ _)
>		 = Fail $ "CE not Elim for makeCyclicElim: " ++ show e_name

>		chg_c chg_f (CE c@(FnDef a rhs _) ty)
>		 = let new = chg_f rhs 
>		   in new `fseq`
>		      let new_atom = compileReductionAtom c_name new 
>		      in new_atom `fseq`
>		         Ok (CE (FnDef a new new_atom) ty)
>		chg_c chg_f (CE _ _)
>		 = Fail $ "CE not Const for makeCyclicElim: " ++ show c_name

>		replace n t = replaceTerminal (chg_const n t)
>		chg_const n t (ContextVar i) | i == n = Just t
>		chg_const n t (Const i _)    | i == n = Just t
>		chg_const n t (Elim i _)     | i == n = Just t		-- conflict ? 
>		chg_const n t _                       = Nothing



---
`check_and_add_comp_rule' checks that type is acceptable and adds to ctxt. 

> check_and_add_comp_rule :: E_Name -> TranslateFn -> CompRuleFunction -> IM
> check_and_add_comp_rule e_name transl (CRF i a t ty)
>  = do
>		c <- get_M_ 

>		p_Normal $ "Checking rule " ++ show i

>		(TT t2 ty2) <- embed $
>					   do
>						  ty1 <- transl c ty
>						  t1  <- transl c t
>						  typeCheckRawKind c t1 ty1

>		p_Normal $ "Now changing rule " ++ show i

>		chg_M_ (chgCtxt i $ to_function (replaceTerminal chg_elim_to_f_elim t2))

>		c2 <- get_M_
>		p_Normal $ "Compiling E Rule for " ++ show i ++ "\n"
>		cr_trace_m $ "Term= " ++ show_rule c2 t2  ++ "\n"
>		cr_trace_m $ "Type= " ++ show_term c2 ty2 ++ "\n"
>		return ()
>    where
>		to_function t (CE (Hyp _) ty) 
>		 = t `fseq` 
>		   let new_atom = compileReductionAtom i t 
>		   in new_atom `fseq` 
>		      Ok (CE (FnDef a t new_atom) ty)
>		to_function _ (CE _ _)    
>		 = Fail $ "CE not Hyp for Constr: " ++ show i

>		chg_elim_to_f_elim (Elim n _) | n == e_name = Just $ F_Elim n
>		chg_elim_to_f_elim t          = Nothing



---
show the basics of a comp rule - just wipe out type info to compress expr.

> show_rule c t = show_term c (show_rule_ t)
> show_rule_ (FO v _ t) = FO v (ContextVar $ Plain "_") (show_rule_ t)
> show_rule_ t          = t




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
FOR COMPILATION OF ARB. COMP RULE TERMS

NOT WORKING!

#if 0

DISABLED! DISABLED! DISABLED! DISABLED! 

%-------------------------------------------------------------------------------

This module is for the really complex comp rule handling.

NOT IMPLEMENTED - opted for a quick solution that assumes linear patterns.

POSTPONE this - since most of the work is checking and syntactic manipulation


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

> compileElimRules :: Name -> [CompRule] -> CM ()
> compileElimRules e rs 
>  = do
>		c <- get_M_
>		-- check if E is known and NOT an elim already
>		-- need also check that constrs are not already processed?
>		-- might want to build terms in Term (because E's S_Type lost?)
>		-- NB in this case we could use context to load bindings... (hacky!)

>		ok <- findCtxt e c
>		catch_M_ (not_right_form c) $ 
>		do 
>			(CE Hyp e_type) <- embed ok
>			let e_bindings = fst $ un_DP e_type
>			let crfs = map (compile_e_rule e_bindings) rs
>			...
>    where
>		not_right_form c _ 
>		 = print_M_ $ "Elim Op " ++ e ++ " not a Hyp:\n" >>
>			print_M_ (longRender $ showCtxtEntry c e)

---
translate :: CompRuleFunction -. 

---

NEED name and type for the specific comp rule instance...
so hence need ctxt here? (for easiness)

name can be picked from the comp rule.
	NB must have only one occurrence!! - check at top? 

NB don't need the additional args, if not going to PM them! (can take from
E_type -- ALTHOUGH need to be careful about the user wanting to rebind them
in some funny rule?)

---

> compile_e_rule :: [DPBinding] -> CompRule -> OkF? CompRuleFunction
> compile_e_rule e_bindings (CompRule l r k)
>  = CRF name arity
>    where
>		name = 
>		arity = FOR CONSTR ONLY !  
>		fn
>		ty = un_dp e_bindings k

----
Decomposing a comp Rule to expected format.
	-- checking of arity done at top level.
	-- ok if translated & types check? (but UNBOUND VARS!)

> flatten_ap :: SimpleTerm -> [SimpleTerm]
> flatten_ap (Ap_S l r) = flatten_ap l ++ [r]
> flatten_ap t          = [t]

> name_of (Name_S n) = Ok n
> name_of t          = Fail $ "not a name: " ++ show t

> data CR_Data
>  = CR_Data 
>		E_Name 
>		E_Arity 
>		C_Name 
>		[C_Arg]		-- infer arity.


> decompose :: SimpleTerm -> OkF CR_Data
> decompose t
>  = do
>		let e_ap = flatten_ap t
>		assert (length e_ap >= 2) ("Short E Application: " ++ show t)
>		e_name <- name_of (head e_ap)
>		let e_arity = length e_ap - 1
>
>		let c_term = last e_ap
>		let c_ap = flatten_ap c_term
>		assert (length c_ap >= 1) ("Short C Application: " ++ show c_term)
>		c_name <- name_of (head c_ap)
>		
>		return $ CR_Data e_name e_arity c_name (tail c_ap)

#endif
