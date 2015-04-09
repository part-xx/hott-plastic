> {-# LANGUAGE NPlusKPatterns #-}
> module Inductive where

Building inductive type things

> import Data.List(mapAccumL)

> import Base
> import SimpleTerm
> import SimpleToTerm(translate_db)
> import Terms
> import SharedSyntax(unbind) -- TMP!
> import TermOps(makeContextVar_Binding,replaceTerminal)
> import TermReduction(compileReductionAtom, shiftUnderOneBinder)
> import Context
> import ContextCmds
> import Printing

> import CommandTypes hiding (InductiveParameter(..))
> import qualified CommandTypes (InductiveParameter(..))

> import InductiveAux hiding (e_name)		-- messy.
> import qualified InductiveAux(e_name)

> import Debugging (trace)

> import ComputationRules (addElimRules, makeElimCyclic)




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
To add:
	- theorems etc on the types
	- support for inductive types, eg named Rec and Iter

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

> processInductiveOld :: TopCommand -> IM 
> processInductiveOld i@(Inductive 
>				d@(Declaration [name] kind) 
>				flags 
>				pars 
>				constrs) 

>  = do 
>		check_constr_names_are_free constrs

>		-- add generalised names to ctxt.
>		-- NB some repetition with inductive_aux
>		let ps_bs = [ (Bound n,ty) | (Declaration ns ty) <- pars, n <- ns ]
>		let ps_nms = [ n | (Bound n,_) <- ps_bs ]
>		declare_family name (un_bs ps_bs kind) constrs (rebuild i)

>		let generalise = un_bs ps_bs . replaceName_S name (ap_nms name ps_nms) 
>		declare_consts generalise constrs

>		let flat_constrs = flatten_decls constrs
>		(e_decl,e_arity,ers) 
>		     <- embed $ buildElimRule (is_rel flags) name kind pars flat_constrs
>		-- print_M_ $ "build elim rule\n"

DISCHG after here? 

>		let e_name = InductiveAux.e_name name
>		declare_consts id [e_decl]
>		-- print_M_ $ "declared elim rule\n"

>		addElimRules translate_db e_name e_arity ers
>		-- print_M_ $ "added elim rules\n"

<>		let c_names = decl_names constrs
<>		add_useful_defs flags name e_name c_names

<>		discharge_over_inductive e_arity pars

<>		makeElimCyclic translate_db e_name ers
<>			-- redo the type of E_X to reflect changes in const terms.

Cyclic Transl. Example of sigma.
	constr sigma can occur as arg of C_Sigma, so when reduced should be as
	per proper Const. 

%---------------------------------------

> processInductiveOld c 
>  = fail_with_msg "Dispatch error - Not (Inductive ...)"

---

> rebuild (Inductive d flags pars constrs) 
>  = "Inductive " ++ unwords (map show_decl pars) ++ " " ++ show_decl d 
>    ++ " " ++ unwords (map show flags)
>    ++ " Constructors " ++ unwords (map show_decl constrs)
>    where
>	show_decl (Declaration ns kind)
>	 = concat [ "[" ++ show name ++ ":" ++ showNicely kind ++ "]"
>	          | name <- ns ]





%-------------------------------------------------------------------------------
%-------------------------------------------------------------------------------

> processInductiveLift :: TopCommand -> IM 
> processInductiveLift i@(Inductive 
>				d@(Declaration [name] kind) 
>				flags 
>				pars 
>				constrs) 

>  | elem CommandTypes.AlsoLift flags
>  = do 
>		check_constr_names_are_free constrs

>		-- add generalised names to ctxt.
>		-- NB some repetition with inductive_aux
>		let ps_bs = [ (Bound n,ty) | (Declaration ns ty) <- pars, n <- ns ]
>		let ps_nms = map (unbind.fst) ps_bs
>		declare_family name (un_bs ps_bs kind) constrs (rebuild i)
>		let generalise = un_bs ps_bs . replaceName_S name (ap_nms name ps_nms) 
>		declare_consts generalise constrs

>		let flat_constrs = flatten_decls constrs
>		(e_decl,e_arity,ers) <- embed $ buildElimRule (error "NYI-alsolift") name kind pars flat_constrs

DISCHG after here? 

>		c_decl <- embed (buildLiftConstrs name kind pars flat_constrs)
>		declare_defs c_decl

>		let e_name = InductiveAux.e_name name
>		declare_consts id [e_decl]

>		addElimRules translate_db e_name e_arity ers

<>		let c_names = decl_names constrs
<>		add_useful_defs flags name e_name c_names

<>		discharge_over_inductive e_arity pars

<>		makeElimCyclic translate_db e_name ers
<>			-- redo the type of E_X to reflect changes in const terms.

>  | otherwise = return ()

Cyclic Transl. Example of sigma.
	constr sigma can occur as arg of C_Sigma, so when reduced should be as
	per proper Const. 

%---------------------------------------

> processInductiveLift c 
>  = fail_with_msg "Dispatch error - Not (Inductive ...)"

%-------------------------------------------------------------------------------
%-------------------------------------------------------------------------------

> liftName :: Name->Name
> liftName (Plain n) = Plain $ "L_" ++ n

> mapTermName :: (Name->Name) -> SimpleTerm -> SimpleTerm
> mapTermName f = lift
>  where
>   lift t@(Name_S (Plain "Type")) = t		-- hacked.

>   --lift (Ap_S e@(Name_S (Plain "El")) t) 
>   -- = Ap_S e (Ap_S (Name_S "M") (lift t))
>   lift (Name_S name) = (Name_S (f name))
>   lift (Ap_S t1 t2) = Ap_S (lift t1) (lift t2)
>   lift (Pars_S t) = Pars_S $ lift t
>   lift t = error $ "Missing case in mapTermName, " ++ show t

<>   lift (Let_S n t1 t2) = Let_S (f n) (lift t1) (lift t2)
<>   lift (Bd_S v (Just n) t1 t2) = Bd_S v (Just (f n)) (lift t1) (lift t2)
<>   lift (Bd_S v Nothing t1 t2) = Bd_S v Nothing (lift t1) (lift t2)

%-------------------------------------------------------------------------------
%-------------------------------------------------------------------------------

> processInductive :: TopCommand -> IM 
> processInductive a@(Inductive 
>			d@(Declaration [name] kind) 
>				flags 
>				pars 
>				constrs) 
>  = do
>	processInductiveOld a
>	-- processInductiveLift (Inductive (liftDeclaration d)
>	-- 		flags pars (map liftDeclaration constrs))

<>	declare_defs [Definition (liftName name) 
<>			[] (Name_S (mliftName name name)) Nothing]

<>	let c_names = decl_names constrs
<>	sequence_ (map (add_lifted_constr flags name) c_names) -- lifted constructors

> 	where
>       	liftDeclaration (Declaration [constr] kind) = 
>                          (Declaration [mliftName constr constr] 
>				(mapTermName (mliftName name) kind))

> 		mliftName name n | name == n = Plain $ "L0_" ++ show n
> 		mliftName name n | otherwise = liftName n


%-------------------------------------------------------------------------------
%-------------------------------------------------------------------------------

<> add_lifted_constr :: [InductiveParameter] -> Name -> Name -> CM ()
<> add_lifted_constr ps name c_name
<>  = do
<>		std
<>		ctxt <- get_M_
<>		let ok = findCtxt c_name ctxt,

<>		term <- embed ok
<>		declare_defs [Definition (unmliftName c_name) [] (liftConstr name term) Nothing]

<>		if (isOk ok) 
<>			then return () -- opt
<>			else return ()
<>    where
<>		std = declare_defs []  -- [rec n e, iter n e]
<>		-- opt = declare_defs $ ind n e : map (is_pred n e) cs
<>		unmliftName name = "Linv_" ++ name

<> liftConstr :: Name->SimpleTerm->SimpleTerm

<> liftConstr name t = t

%-------------------------------------------------------------------------------
`check_constr_names_are_free'
  - check that none of the constr names occurs in any of their types
  - this is a requirement of well-formedness

> check_constr_names_are_free :: [Declaration] -> IM
> check_constr_names_are_free ds
>  = case (occurring_constr_names ds) of
>		[]   -> return ()
>		ntys -> fail_with_msg $ bad_names (map (show) ntys)
>    where
>		bad_names ns = "Names (" ++ unwords ns ++ ") occur in constr types."

> occurring_constr_names ds
>  = [ (n,ty) | n <- ns, ty <- tys, n `occursIn` ty ]
>    where
>		(ns,tys) = unzip [ (n,ty) | Declaration ns ty <- ds, n <- ns ]


%---------------------------------------
Map flags to relation/standard

> is_rel :: [CommandTypes.InductiveParameter] -> Relation_or_what
> is_rel fs
>  | CommandTypes.Relation `elem` fs    = InductiveAux.Relation 
>  | CommandTypes.Relation_LE `elem` fs = InductiveAux.Relation_LE
>  | otherwise                          = Standard


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
`add_useful_defs' - add fn defs based on Elim for:
	* plain recursion on a type t
	* iteration of some function on some type t
	* induction on Prop (if Prop is declared in type-safe manner)
	* is_X predicates on the constructors (ditto for Prop)

	* ANY SUGGESTIONS FOR MORE? (apart from inversion theorems?)

The defs which need Prop will succeed if Prop is defd in such a way that the
definitions type check. 

<> add_useful_defs :: [InductiveParameter] -> Name -> Name -> [Name] -> CM ()
<> add_useful_defs ps n e cs
<>  = do
<>		std
<>		c <- get_M_
<>		let ok = findCtxt "Prop" c		-- prerequisite! 
<>		if (isOk ok) 
<>			then return () -- opt
<>			else return ()
<>    where
<>		std = declare_defs [rec n e, iter n e]
<>		-- opt = declare_defs $ ind n e : map (is_pred n e) cs

<> rec n e 
<>  = -- trace ("Adding Rec " ++ n ++ "\n" ++ show def ++ "\n\n")
<>    Definition (n++"Rec") [] def Nothing 
<>    where
<>		def = Bind_S By_FO (Bound (Plain "t")) Type_S
<>				(Ap_S (Name_S e) (FO_S Nothing (Name_S n) (Name_S "t")))

<> ind n e 
<>  = Definition (n++"Ind") [] def Nothing 
<>    where
<>		def = FO_S (Just "P") 
<>				(DP_S Nothing (Name_S n) (Name_S "Prop"))
<>				(Ap_S (Name_S e) (Name_S "P"))

---
NOT DONE YET.
	- needs info about I sch. 

<> is_pred n e c = Definition ("is_"++c) [] Type_S Nothing
<> iter n e = Definition (n++"Iter") [] Type_S Nothing

[    nat_iter = [t|TYPE][x:t][f:t->t]nat_rec x ([_:nat]f)
             : {t|TYPE}t->(t->t)->nat->t
]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
AUX

`declare_consts' 
  - declare (with name-free check), plus rewrite types eg for dischg.

> declare_consts :: (SimpleTerm -> SimpleTerm) -> [Declaration] -> CM ()
> declare_consts fn ds
>  = do
>		add_defs_decls dds
>    where
>		dds = [ Def_Decl $ Right $ Declaration ns (fn ty) 
>		      | (Declaration ns ty) <- ds ]

---

> declare_family :: Name -> SimpleType -> [Declaration] -> String -> CM () 
> declare_family n ty cs info
>  = do
>	ty' <- use_M_ $ \c -> translate_db c ty
>	add_if_unused n (CE (IndFamily cs_nms (ContextVar n) info) ty')
>    where
>	cs_nms = [ n | (Declaration ns _) <- cs, n <- ns ] 

---
`declare_defs' - 

> declare_defs :: [Definition] -> CM ()
> declare_defs = add_defs_decls . map (Def_Decl . Left)

---
`flatten_decls' - buildElimRule expects just one name per decl.

> flatten_decls ds = [ Declaration [n] ty | (Declaration ns ty) <- ds, n <- ns ]

---
`decl_names'

> decl_names ds = concat [ ns | (Declaration ns _) <- ds ]



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#ifndef __HASKELL98__
#define sequence_ sequence
#endif

> discharge_over_inductive :: Arity -> [Declaration] -> IM
> discharge_over_inductive e_arity pars
>  = sequence_ $ 
>    [ d_i arity n | (arity,n) <- zip [e_arity..] nms ]
>    where
>		nms = [ n | (Declaration [n] _) <- reverse $ flatten_decls pars ]

do in reverse, one at a time.

parameters should have free names (so renaming hasn't occurred).



---
why not using findInCtxt?


> -- crude lookup, plain split.
> partition_ctxt n (Ctxt es)
>  = case (break (\x -> fst x == n) es) of
>		(ps, [])   -> error $ show n ++ " not found. REDO WITH ASSERT?"
>		(ps, c:cs) -> (ps, snd c, cs)


---

discharge param from all of the post-ctxt.
all occurrences of entities in the post-ctxt need an extra parameter.

> d_i :: Arity -> Name -> IM
> d_i e_arity n
>  = do
>		c <- get_M_
>		let (post_c, CE cek ty, pre_c) = partition_ctxt n c

>		assert_p (isHyp cek) 
>		         ("Inductive parameter (" ++ show n ++ ") must be Hyp")
>
>		let d_nms = map fst post_c
>			-- ids being rewritten - any mention now needs an extra var, n
>
>		let post2 = map (lift e_arity d_nms (n,ty)) $ reverse post_c
>		
>		set_M_ (Ctxt $ reverse post2 ++ pre_c)

>		c' <- get_M_					-- hacky force.
>		showCtxt Show_Something c' `seq` return () 
>		-- print_M_ $ "Done dischg on " ++ show n ++ "\n"



NOTE: could use some of the functionality for CUT., eg over_CE
	but that would need to be generalised.


---
`lift'
  - add param 'n' to all occs. of d_nms in the ctxt entry.
  - includes extending types with additional DP binding

> lift 
>  :: Arity -> [Name] -> (Name,Type) -> (Name,Ctxt_entry) -> (Name,Ctxt_entry)

> lift e_arity d_nms (n,ty_n) (ce_nm, CE hyp@(Hyp _) ty)
>  = (ce_nm, CE hyp new_ty)					-- only adjust the type term.
>    where
>		new_ty = makeContextVar_Binding DP n ty_n $ add_var d_nms n ty

> lift e_arity d_nms (n,ty_n) (ce_nm, CE (ElimOp a _) ty)
>  = (ce_nm, CE (ElimOp (a+1) (Elim ce_nm (a+1))) new_ty)		
>										-- adjust the type and incr.  arity
>    where
>		new_ty = makeContextVar_Binding DP n ty_n $ add_var d_nms n ty


A Const can occur in its rhs, thus this modification MUST be cyclic.
1feb99 - not necessarily - since there is a makeElimCyclic op. now.
CHECK!

> lift e_arity d_nms (n,ty_n) (ce_nm, CE (FnDef a rhs _) ty)
>  = let new_rhs = rhs'
>    in new_rhs `fseq`
>       let new_arity = a + 1 
>       in new_arity `fseq` 
>          let new_atom = compileReductionAtom ce_nm new_rhs
>          in new_atom `fseq`
>             (ce_nm, CE (FnDef new_arity new_rhs new_atom) new_type)
>    where
>		new_type = makeContextVar_Binding DP n ty_n $ add_var d_nms n ty 
>			-- just add the outer binding.

>		e_pars = e_arity - 1
>			-- we want all of the E args but one (the constructor)

>		rhs' = replaceTerminal (make_cyclic rhs') $
>		       makeContextVar_Binding         FO n   ty_n $ 
>		       internal_bind_anonymous e_pars FO     ty_n $
>		       add_var d_nms n                            $
>		       rhs
>			-- NB arity of comp rule increases by TWO.
>			-- hence adding two binders.
>			-- the outer one is regarded as main binder 
>			-- the inner one is made anonymous (ie, FO "_" ....)

>		-- REMOVE THIS SOON.
>		make_cyclic new (Const n t) | n == ce_nm = Just $ Const n new
>		make_cyclic new _                        = Nothing


> lift e_arity d_nms (n,ty_n) (ce_nm, CE cek ty)
>  = error $ "Bad CEK in lift (" ++ show n ++ ") - " ++ show (ce_nm, cek)


%---------------------------------------
Inserting a binder inside the function

`internal_bind_anonymous' 
  - inserts a binder (as function) inside a term, shifting the dB vars.
  - it doesn't have to change (eg) ContextVars to IVars, since the binding
	var is "_" 
  - it DOESN'T 

> internal_bind_anonymous e_arity cf ty t
>  = replace_binders $ cf Underscore ty $ shiftUnderOneBinder r
>    where
>		(bs, r) = un_bind e_arity t
>		replace_binders t = foldr id t bs

---
`un_bind' 
  - takes off i binders
  - the term SHOULD have at least i binders (by definition), 
	so crash if not - it's a programming error.
  - (NB un_bind should only produce all [FO] or all [DP]...)

> un_bind 0 t = ([], t)
> un_bind (i+1) (FO n ty t) 
>  = let (bs,r) = un_bind i t in ((FO n ty) : bs, r)
> un_bind (i+1) (DP n ty t) 
>  = let (bs,r) = un_bind i t in ((DP n ty) : bs, r)
> un_bind (i+1) t 
>  = error $ "un_bind " ++ show (i+1) ++ " on term: " ++ crude_print t



---
add a var to any name which is being rewritten.

 - BUT NEED ADJUST THE CONSTS. 
 - WARNING - mass reduplication could be expensive. try to go through Ctxt?
	BUT some of the cyclicity might be broken? should do it during the Comp?  

OR discharge before compile? - which means, no context help. 

(NB 1feb99 - makeElimCyclic - could solve problems)

> add_var :: [Name] -> Name -> Term -> Term
> add_var d_nms nm
>  = replaceTerminal foo 
>    where
>		app_fn = \t -> Ap t (ContextVar nm)
>		recurse = add_var d_nms nm 

>		foo t = nameFromTerm t >>= \n -> 
>		        if n `elem` d_nms then redo_term t else Nothing

>		redo_term t@(Elim i a)
>		 = Just $ app_fn (Elim i (a+1))
>		redo_term t@(ContextVar _) 
>		 = Just $ app_fn t		-- should be IND? 
>		redo_term t@(Const i r)  
>		 = Just $ -- trace ("DISCHG RHS of (" ++ i ++ ")\n") $
>		          app_fn (Const i $ recurse r)
>		redo_term _                
>		 = Nothing

CV eg for Pi, maybe should use Ind marker? 
