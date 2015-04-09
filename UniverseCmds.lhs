> module UniverseCmds (process) where

commands that involve Universes.

> import BaseClasses(Forceable(..), Fallible(..), Embeddable(..))
> import Base(OkF(..), isOk, elimOk, fromOk)
> import StateMonads(print_M_, use_M_)

> import Debugging(ftrace, hack_show, show_term)

> import CommandTypes(UnivOpt(..), TopCommand(..))

> import Terms(Term(..), Type(..), FlatTerm(..))
> import SharedSyntax(Name(..), BoundName(..), Extension(..))
> import Terms(build_ap, isCAST, isIVar, isAtom, pretty_crude_print)
> import TermOps(subtrees, replaceTerminal, terminals, un_DP)
> import TermReduction(first_IVar, compileArbitraryTerm, shiftUnderOneBinder)

> import Context
> import ContextOps(univLookup)


> import ContextCmds(add_if_unused)

> import SimpleTerm(SimpleTerm(Name_S))
> import SharedSyntax(univ_separator)
> import SimpleToTerm(translate_db)
> import TypeInference(inferType, TypedTerm(..))

> import Universes

> import Reduction

#ifndef __HASKELL98__
#define fmap map
#endif


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

> process :: TopCommand -> IM 

> process i@(InfiniteUniv _ _ _ _)      = process_infinite_univ i
> process a@(AddToUniverse _ _)         = process_add_to_universe a

> process a@(AddToPseudoUniverse _ _ _) = process_add_to_pseudo_universe a

> process _ = error "Process, unexpected universe cmd"

%-------------------------------------------------------------------------------
Experimental:
 - adding type formers for a fixed user-defined universe.
 - eg Sigma inside (V,S).
 - ASSUMING that the constructions are identical to those of Type^i

To do:
 - type check the terms. - although better to def. as names? 
 - BUT - just terms means that can really get generic, not constrained
 - check the typing - univ is kind, decode is K->K only
 - then pass in to foo/bar as fixed terms. 
 - look at strictness of these, ie so only do once? 

> process_add_to_pseudo_universe (AddToPseudoUniverse u_name u_decode it_name)
>  = error "AddToPseudoUniverse NYI"



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Adding Names of Inductive Types to (open) universes.

 - for univ base name u, and inductive family 'family'
 - add dynamic generator for names to produce names <family>_<u>^i, where i
   is not smaller than the minimum level allowed (eg by constraints in the
   kinds of the constructors). 

 - the i'th name has all mentions of kind Type changed to Type^i, with 
   appropriate T^i wrappers where needed (eg in DPs.)

 - most (sensible) types will have no such restriction.


> process_add_to_universe (AddToUniverse u family)
>  = do
>		use_M_ $ findCtxt (Plain u)
>		(u_NAME,_) <- embed $ split_uid (Plain $ u ++ "0")
>								-- add 0 -> ensure valid id

>		(family_ty, min_cs_level) <- ind_family_info family
>			-- NB in analysing the constrs, we might pick up constraints 
>			-- from the parameters or family arguments? 
>			-- BUT I assume this is ok, since we could not go below anyway? 
>		let min_level = min_cs_level		-- PROVISIONAL.

>		let ty = ce_dummy_type "family-name-generator"
>		let base_nm = Plain $ show family ++ "_" ++ u_NAME ++ [univ_separator]

>		let fn = generate_family_nm min_level family family_ty

>		add_if_unused base_nm (CE (Dynamic fn) ty)

---

Fam Nm is TMP! 

> t0 = (DP (Bound $ Plain "A") Type $ DP (Bound $ Plain "B") Type Type)

> gfr t 
>  = case generate_family_nm 0 (Plain "Foo") t "Foo" 0 of
>		Ok t -> putStr $ show_term emptyCtxt $ 
>				hnf emptyCtxt $ 
>				Ap (Elim (Plain "T") 1) 
>				(ap2 t (ap2 t (pv "a") (pv "b")) (pv "c"))
>    where
>		pv = Var . Plain 
>		ap2 t x y = Ap (Ap t x) y

> gfn t
>  = case generate_family_nm 0 (Plain "Foo") t "Foo" 0 of
>		-- Ok (Ext (HardCast (Const _ _ c) _)) -> c
>		Ok (Ext (HardCast (Const _ c) _)) -> putStr $ pretty_crude_print c ++ "\n\n" ++ show_term emptyCtxt c

> generate_family_nm :: MinUniverseLevel -> Name -> Type -> Mk_U (OkF Term) 
> generate_family_nm min nm if_ty p i
>  | i < min 
>  = Fail $ "generate_family_nm: " ++ show i ++ " is below minimum " ++ show min
>  | otherwise
>  = do
>		tYPE <- mk_u_NAME "Type" i
>		elim <- mk_u_elim "Type" "T" i
>
>		-- ftrace ("GFN: " ++ show (tYPE, elim) ++ "\n") $ return ()
>		-- ftrace ("IF_T: " ++ show (if_ty) ++ "\n") $ return ()
>		let if_ty_i = foo (El tYPE) elim if_ty

>		let if_ty'  = mk_DP (fst $ un_DP if_ty) stop_bar
>		let if_t    = bar (El tYPE) elim (ContextVar nm) if_ty' if_ty_i
>				-- try to merge these!
>
>		-- let if_t' = mk_DP (fst $ un_DP if_ty_i) if_t
>		let if_t' = if_t
>		-- ftrace ("TYPE:\n" ++ pretty_crude_print if_ty_i ++ "\n") $ return ()
>		-- ftrace ("TERM:\n" ++ pretty_crude_print if_t    ++ "\n") $ return ()
>		-- ftrace ("term:\n"   ++ show if_t      ++ "\n") $ return ()
>		-- ftrace ("c-term:\n" ++ hack_show if_t ++ "\n") $ return ()

>		return (Ext (HardCast (Const (mk_uid p i) if_t') if_ty_i))
>    where
>		a = ()

---

> mk_DP bs end = foldr (\(n,ty) -> DP n ty) end bs



---
Rewriting the kind to its form in universe i.

eg Pi : (A:Type)(B:A->Type)(C:((x:A)B x) -> Type)Type
   pi^i : (A: El Type^i) (B: T^i A -> El Type^i)El Type^i
		= [A: El Type^i] [B: T^i A -> El Type^i]Pi (T^i A) ([x:T^i A]T^i (B x))

Ideas
 - replace IVAR x with FO which does lifting. 
	- but this isn't really a correct reduction.
Current Idea
 - all occurrences of Type mapped to (El Type^i)
 - all (El $ IVar x) -> (El $ T^i $ IVar x)
 - then build the reduction rule term by induction on the new type.


> foo tYPE elim Type
>  = tYPE

> foo tYPE elim (El t) 
>  | not $ any isIVar $ terminals t		-- no IVars.
>  = El t
>  | otherwise
>  = El (Ap elim $ foo tYPE elim t)

> foo tYPE elim (DP v ty t)
>  = DP v (foo tYPE elim ty) (foo tYPE elim t)

> foo tYPE elim (Ap l r)
>  = Ap (foo tYPE elim l) (foo tYPE elim r)

> foo tYPE elim t
>  | isAtom t  = t
>  | otherwise = error $ "Univ gen, unexpected type component, " ++ show t

---

<> bar :: Term -> Type -> Term

> stop_bar = Var $ Plain stop_bar_string		-- hacky! 
> stop_bar_string = "stop bar" :: String

> bar tYPE elim f (Var (Plain s)) _
>  | s == stop_bar_string = f 		-- termination.

> bar tYPE elim f (DP v ty t) (DP _ tyi ti)
>  = FO var tyi $ bar tYPE elim (Ap (shiftUnderOneBinder f) arg) t ti
>    where
>		arg = bar tYPE elim first_IVar ty (shiftUnderOneBinder tyi)
>		var = if v == Underscore then (Bound (Plain "x")) else v

QQ: need to shift the ty too?
shift on tyi is needed for Pi - but am not sure why! (need shift elsewhere?)

> bar tYPE elim f Type el_type_i
>  = Ap elim f

> bar tYPE elim f (El t) (El ti) 
>  = bar tYPE elim f t ti

> bar tYPE elim f i@(IVar _) tyi
>  = first_IVar

> bar tYPE elim f t tyi
>  | not $ any isIVar $ terminals t		-- this term is constant
>  = first_IVar							-- doesn't need rewrite.

> bar tYPE elim f ap@(Ap l@(IVar _) _) tyi
>  = ap
>	-- special case for Psi, need to generalise it properly! 

> bar tYPE elim f t tyi
>  | otherwise
>  = error $ "BAR missing case, " ++ show (t,tyi)	-- don't know yet.	





---
`ind_family_info' 
  - obtain type of the (type) constructor of the ind family
  - Do fam. arguments make a difference? 
  - plus the min. level universe at which it can be placed (which is set by
	the universes used in its construction).

YES: sep kind.

> type MinUniverseLevel = UniverseLevel

> ind_family_info :: Name -> CM (Type, MinUniverseLevel)
> ind_family_info n
>  = prepend_msg "Analysing Inductive Family for Universe Info." $
>    do
>		(CE cek ty) <- use_M_ $ findCtxt n
>		cs <- case cek of
>				fam@IndFamily{}-> return (constrs fam)
>				_              -> fail_with_msg $ show n ++ ": not inductive family"
>		ls <- fmap concat $ mapM find_universe_levels cs
>		return (ty, if null ls then 0 else minimum ls)

---
Oct00, want more reliable way of checking for universes.

> find_universe_levels :: Name -> CM [UniverseLevel]
> find_universe_levels n
>  = do
>		CE _ ty <- use_M_ $ findCtxt n
>		let expand_def (GlobalDef _ rhs) = Just rhs
>		    expand_def (LocalDef  _ rhs) = Just rhs
>		    expand_def _                 = Nothing
>		let expanded_ty = replaceTerminal expand_def ty
>		let casts = subtrees isCAST expanded_ty
>		let ls = [ elimOk (const 0) snd (split_uid i) 
>		          | (Ext (HardCast (ContextVar i) _)) <- casts ]
>		return ls 


---

<>		t' <- use_M_ (\c -> translate_db c t)
<>
<>		(TT t'' ty_t') <- use_M_ (\c -> inferType c t')
<>
<>		case ty_t' of
<>			Type -> add_name_for_type u_ty (n,t'')
<>			DP _ _ _ -> do
<>							let (bs,m) = un_DP ty_t'
<>							let u_ty' = foldr (\(n,ty) -> DP n ty) u_ty bs
<>							-- let vs = take (length bs) map IVar [db_start ..]
<>							--
<>							let arity = length bs
<>							let fn as bs = E t'' (as ++ bs)	-- HACK
<>							let t0 = t''		-- this only for display! HACK
<>							-- let tc = Closed (n++"*") arity t0 fn 
<>							let cek = FnDef arity t0 (F_Const n arity fn)
<>							add_if_unused n $ CE cek u_ty'
<>
<>			_ -> fail_with_msg "foo"

<> add_name_for_type u_ty (n,t)
<>  = do
<>		cek <- embed $ prepend_msg "AddToUniverse, " $ compileCEK n t
<>		add_if_unused n $ CE cek u_ty


%-------------------------------------------------------------------------------
Finite Universes

 - process from outside in - this is correct order.

<> process (FiniteUniv o n cts)
<>  = do
<>		add_decl (Declaration n UnivType)
<>		error "NYI"
<>		error "NYI"


%-------------------------------------------------------------------------------
Infinite Universes.

include univ_sep in id - prevent lookup.

<> process (InfiniteUniv os ((_,_),_) _ _)

> process_infinite_univ (InfiniteUniv _ _ _ _)
>  = do
>	let [nAME,decode,name,step] = words "Type T type t"

>	add nAME   (Dynamic $ mk_u_NAME)             Type
>	add decode (Dynamic $ mk_u_elim nAME)        e_type
>	add name   (Dynamic $ mk_u_name nAME)        n_type
>	add step   (Dynamic $ mk_u_step nAME decode) c_type

>	-- need Prop defined for this - else will fail when we add the name.
>	let pROP = ContextVar (Plain "Prop")		-- 

>	-- prop : Type^0, T^0(prop) = Prop : Type
>	-- special case, not inductive type. hence do directly
>	let prop_name = Plain "prop"
>	let u_ty = El $ fromOk $ mk_u_NAME nAME 0
>	let cek = FnDef 0 pROP (F_Const prop_name $ \_ -> E pROP [])
>	add_if_unused prop_name $ CE cek u_ty
>    where
>	add n cek ty = add_if_unused (Plain $ n ++ [univ_separator]) $ CE cek ty
>	n_type = ce_dummy_type $ "u-name"
>	e_type = ce_dummy_type $ "u-elim"
>	c_type = ce_dummy_type $ "u-step"

Type^i first. 
then T^i - as eliminator.
t^i AND small names are constructors for this. 


