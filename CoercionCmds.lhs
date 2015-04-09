> module CoercionCmds (addCoercion, showCoercions) where

top-level utils to manipulate coercions.

eg adding and displaying

should not be big module...

> import Data.List(isPrefixOf, tails)
> import Data.Maybe(fromMaybe, isJust, fromJust, maybeToList, catMaybes)
> import Pretty(text, nest, empty, (<+>), ($$))
> import PrettyAux(longRender, stext)

> import BaseClasses(OkF(..), Embeddable(..), Fallible(..), Permutable(..))
> import BaseClasses(Forceable(..))
> import BaseTypes(fromOk)
> import Base(assert_p)
> import StateMonads

> import CommandTypes

> import GlobalOptions(p_Normal, p_Everything)
> import DebugOptions(traceOnDebugFlag)
> import Debugging(show_term, pretty_crude_print)

> import SimpleTerm(un_bs, un_bs_FO, Name(..))
> import SimpleToTerm(translate_db)

> import Context 

<> import Coercions		-- what from here? 

> import Terms(Term(..), Type(..), TypedTerm(..), Binding(..))
> import Terms(nameFromTerm, isDef, build_ap, isDP, isEl)
> import TermOps(un_DP, replaceTerminal, bindingGroups)
> import TermReduction (first_IVar, shiftUnderOneBinder, reduceAppliedAbstrs, db_start)

> import Printing(showTerm, showTerm_, showTypedTerm_)

> import TypeInference(inferType, typeCheckRawKind)
> import Reduction (dwhnf, etaContract)
> import Convertibility(convertible, unifiable)

> import ConstraintTypeInf(findCoercion)		-- should put elsewhere?
> import qualified ConstraintTypeInf(inferType)

> import SharedSyntax(BoundName(..), Name(..), CoercionName(..))


#ifndef __HASKELL98__
#define fmap map
#endif


> ctr s = traceOnDebugFlag "CO_DECL_TRACING" (s `fseq` s ++ "\n")
> mctr s = ctr s $ return ()


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

NB sharing of K with Coercions module

str of Ctxt entries.

checking of coherence!!!! 


%-------------------------------------------------------------------------------

> addCoercion :: TopCommand -> IM

<> addCoercion (CoercionDecl [] [] t Nothing)
<>  = fail_with_msg "Please use the new style of coercion decl."

---
%---------------------------------------
Notes: 

on what to do about prerequisites.
change to nested syntax - so we don't have to do fiddly analysis? 
representing prereqs as PROPER coercions is the correct thing to do.

then: we type check a term in the extended context, although we build
a conventional FO-term for storage in Co-table. 

NB don't want to represent Co binding in a term!!! (a la McBride...)




%---------------------------------------


> addCoercion (CoercionDecl ps qs t mty)
>  = prepend_msg ("addCoercion: ") $
>    do
>	let params = get_bs ps
>	let prereqs = qs
>	let bindings = params -- now includes the prereqs.
>	let c_term =       un_bs_FO bindings  t
>	let c_type = fmap (un_bs    bindings) mty

>	t'  <- use_M_ $ \c ->              translate_db c  c_term
>	ty' <- use_M_ $ \c -> permute_map (translate_db c) c_type

>	tt <- case ty' of
>			Nothing -> use_M_ $ \c -> inferType c t'
>			Just ty -> use_M_ $ \c -> typeCheckRawKind c t' ty
>	let (TT t ty) = tt
>	let (bs,rty) = un_DP ty

This condition is too strong, eg for pi_2 in Sigma.
<>	let t_params = take (length params) bs
<>	assert_p (all isEl_or_Type $ map snd $ t_params)
<>	         "Coercion parameters may only have kind Type or (El x)"

>	assert_p (length (drop (length bindings) bs) == 1) 
>	         "Coercion term must be DP with a single argument."

>	assert_p (isEl $ snd $ last bs) "Coercion argument must be a Type"
>	assert_p (isEl $ rty)           "Coercion result must be a Type"

>	let t_prereqs = [ b | b@(Bound n,_) <- bs, n `elem` prereqs ]
>	assert_p (all isDP $ map snd  $ t_prereqs)
>	         "Coercion prerequisites may only have DP kind"

>	-- only check coherence on acceptable pattern.
>	test_coherence t (bs,rty)

>	let dummy = Coercion {term = error "no cek yet"}
>	cn@(CoName i) <- add_user_coercion (CE dummy ty)
>	 -- put in dummy, to get name; fill in later.
>	 -- NB shares some code with bind_mvs.

>	let eta_term = etaContract t
>	let eta_term_def = case eta_term of
>						GlobalDef _ rhs -> GlobalDef cn rhs
>						_               -> GlobalDef cn eta_term
>		-- unpack the def, and relabel with our preferred name.

>	let some_mvs = [ Var $ Plain "?" | _ <- bindings ]
>	let term_with_mvs = build_ap eta_term_def some_mvs     -- (c ?...?) 
>	let is_co_rule = not $ null prereqs

>	let cek = Coercion eta_term term_with_mvs 
>							(snd $ last bs) rty is_co_rule eta_term_def
>	chg_M_ $ chgCtxt cn (\(CE _ ty) -> Ok $ CE cek ty)
>		-- fix proper cek value now.

>	print_M_ $ "Added new coercion, id = " ++ show cn ++ "\n"

>	if null prereqs
>		then add_closure i (mv_term cek)
>		else p_Normal $ "Has prerequisites, not adding closures."

>    where
>	get_bs ds = [ (Bound n,ty) | (Declaration ns ty) <- ds, n <- ns ] 



%-------------------
`add_user_coercion'
  - all user-declared coercions have unique ids based on "cx"
  - start with cx1; addCtxt will automatically increment the suffix.
  - 

> coercion_base_name = CoName (CN ["cx1"]) :: Name

> add_user_coercion :: Ctxt_entry -> CM Name
> add_user_coercion ce 
>  = tfm_M_ $ Ok . addCtxt_rn (Bound coercion_base_name) ce

---
`add_derived_coercion'
  - name is derived from the way the coercion was built, so no need to 
	look for unique name

> add_derived_coercion :: CoercionName -> Ctxt_entry -> CM CoercionName
> add_derived_coercion cn ce 
>  = do 
>	chg_M_ $ addCtxt (Bound $ CoName cn) ce
>	return cn

%---------------------------------------

XX create new bound vars for the term, and see if we get a match.
might need shift on rty? 

> test_coherence :: Term -> ([Binding],Type) -> IM
> test_coherence term (bs,rty)
>  = do
>	c0 <- get_M_
>	assert_p (not $ null bs) "coherence test, no binders?"
>	let c_bs = foldl (flip $ \(n,ty) -> addCtxt_BV n ty) c0 bs

>	let bvs = [ IVar i | (_,i) <- zip (init bs) [(db_start + 1) ..] ]
>	let applied_term = Co (build_ap term $ reverse bvs) first_IVar
>		-- Co (term ps) x

>	let c_dom = shiftUnderOneBinder $ snd $ last bs

>	-- test for identity coercion
>		-- first, see if type is unchanged, ie : A -> A.
>		-- then see if extension is identical. (NYI)
>		-- by theory (check), same type implies it IS the identity fn.
>		-- or does this depend on coherence? need to think. 
>	switch_M_ (embed $ convertible c_bs c_dom rty)
>		`try_another_` fail_with_msg "Coercion has form A -> A."
>	p_Normal $ "Coercion isn't A -> A."

>	ok_cx <- raiseOkF_M_ $ embed $ 
>								findCoercion c_bs (TT first_IVar c_dom) rty
>	case ok_cx of
>		Fail _ -> do
>				p_Normal $ "No matching coercion, so coherence ok\n"
>				return ()
>		Ok _   -> do
>				let (TT ct _) = fromOk (fmap snd ok_cx)
>				p_Normal $ "Coercion found: " ++ show_term c_bs ct
>				ok_cvt <- raiseOkF_M_ $ embed $ 
>						unifiable c_bs ct applied_term
>				-- is unification appropriate here? 
>				-- we want GENERAL conv, not unifiable-at-some-cases?
>				case ok_cvt of
>					Ok []  -> do 
>							p_Normal $ "Convertible, so coherent."
>							return ()
>					Ok ss  -> do 
>							p_Normal $ "Unifiable, subst is:"
>							p_Normal $ unlines (map show ss)
>							fail_with_msg "test fixes metavars."
>					Fail m -> do
>							p_Normal $ "NOT Convertible/Unifiable!"
>							fail_with_msg m


%-------------------------------------------------------------------------------
`add_closure'
  - NB only use on terms with non-coercion arguments. 
  - this should be checked by caller. 

  - for coercion c, tries compositions (c' c c'') (c' c) (c c')
  - (NB, restricted so the alg terminates.)

IMPORTANT:
 - using Co instead of Ap to signal want strict (coercion-free) type-checking.

IMPORTANT: (oct-00)
 - am NOT trying composition of coercion rules
 - need to review what should be done with these (or fix mvs binding bug)


> type ClosureDepth = Int
> default_depth :: ClosureDepth
> default_depth = 3 

> add_closure :: CoercionName -> Term -> IM
> add_closure cn co_mv_term
>  = do
>	p_Normal $ "Trying closure for coercion " ++ show cn
>	ns_ces <- add_closure_ default_depth cn co_mv_term
>	p_Normal ""
>	if null ns_ces 
>	  then p_Normal "No New Coercions after closure"
>	  else do
>		sequence [ add_derived_coercion n ce | (n,ce) <- ns_ces ]
>		p_Normal "New Coercions after closure: " 
>		p_Normal $ unlines $ map (("\t"++).show.fst) ns_ces

> add_closure_ 
>  :: ClosureDepth -> CoercionName -> Term -> CM [(CoercionName,Ctxt_entry)]

> add_closure_ 0 i co_mv_term
>  = return []

> add_closure_ cd i co_mv_term
>  = do
>	ctxt_0@(Ctxt es) <- get_M_ 
>	let cs = [ (n, mv_term ce) | (CoName n, CE ce@(Coercion{}) _) <- es, 
>										i /= n, not (co_rule ce) ]

>	-- PRELIM: set up context for checking compositions
>	-- introduce a dummy var, with unknown type, as the range.
>	--
>	let mv_cek nm = CE (M_V (MetaVar nm)) Type		-- ugly!
>	let (ctxt_T, mv_nm) = addCtxt_rn (Bound first_numbered_metavar) 
>	                                 (mv_cek mv_nm) ctxt_0 
>	let bv = Plain "x"
>	let ctxt_Tx = addCtxt_BV (Bound bv) (El $ MetaVar mv_nm) ctxt_T

>	-- try_fn tries a composition and returns a CE if it works.
>	let try_fn:: (CoercionName,Term) -> CM (Maybe (CoercionName,Ctxt_entry))
>	    try_fn nt = do { set_M_ ctxt_Tx; 
>	                     try_composition ctxt_Tx (bv,mv_nm) nt }

>	let cs_ns = map fst cs
>	let not_already_known (cn,_) = cn `notElem` cs_ns

>	-- FIRST: try the simple compositions
>	-- deliberately using Co instead of Ap
>	-- prevents extra coercions being inserted
>	let compose (CN as) (CN bs) = CN $ as ++ bs
>	let c_i = [ (compose a i, Co t $ Co co_mv_term first_IVar) |(a,t) <- cs]
>	let i_c = [ (compose i a, Co co_mv_term $ Co t first_IVar) |(a,t) <- cs]

>	p_Everything $ "Trying compositions for " ++ show (i,cd)
>	p_Everything $ unlines [ "\t" ++ show (fst x) | x <- c_i ++ i_c ]

>	first_comps <- mapM try_fn $ filter not_already_known $ c_i ++ i_c
>	let ok_first_comps = catMaybes first_comps 
>		-- compositions which are acceptable after first composition

>	set_M_ ctxt_0
>	if null ok_first_comps
>	  then p_Everything $ 
>				"Compositions for " ++ show (i,cd) ++ " not accepted"
>	  else p_Everything $ 
>				"Accepted compositions for " ++ show (i,cd) ++ "\n" ++
>	                   unlines [ "\t"++ show (fst x) | x <- ok_first_comps ]
>	ns  <- sequence [ do
>						add_closure_ (cd - 1) n (mv_term cek)
>							`try_M_` report_problem n
>	                | (n,CE cek _) <- ok_first_comps ]     
>		-- declare new compositions
>		-- and try their compositions, bounded by depth
>	return $ ok_first_comps ++ concat ns
>    where
>	report_problem n s 
>	 = do
>		  print_M_ $ "Problem adding composition " ++ show n  
>				      ++ " for coercion " ++ show (i,cd) ++ ":\n\t" ++ s
>		  return []



CAREFUL - which context, which binding levels...
might need to do explicit push/pop (and move this code!) 

NB ctxt is not for use here? ie, by time of test, will be re-bound...

> try_composition 
>  :: Ctxt -> (Name,Name) -> (CoercionName,Term) 
>								-> CM (Maybe (CoercionName,Ctxt_entry))

> try_composition ctxt bv_mv (co_nm,term)
>  = do
>	p_Everything $ "Try composing "++ show co_nm
>	mctr         $ "\t"++ show_term ctxt term
>	case (try_composition_ ctxt co_nm bv_mv term) of
>		Nothing -> return Nothing
>		Just ce -> do
>				ctxt <- get_M_ 
>				let (CE (Coercion t mvt _ _ _ _) ty) = ce
>				mctr $ "Testing coherence for " ++ showTerm_ ctxt t
>				ok <- raiseOkF_M_ (test_coherence t (un_DP ty))
>				case ok of
>					Fail _ -> return $ Nothing 
>					Ok _   -> return $ Just (co_nm,ce)

---
This does the work. 

> try_composition_ 
>  :: Ctxt -> CoercionName -> (Name,Name) -> Term -> Maybe Ctxt_entry
> try_composition_ ctxt cn bv_mv term
>  = case (try_match term Nothing) of
>	Fail m      -> do
>						mctr $ "Composition failed"
>						mctr $ m
>						Nothing
>	Ok (mvs,tt) -> do
>						mctr $ "Before binding, =\n "
>									++ showTypedTerm_ ctxt tt ++ "\n"
>									++ unlines (show_new_mvs mvs)
>						Just $ bind_mvs cn bv_mv mvs tt 
>    where
>	try_match t ty 
>	 = ConstraintTypeInf.inferType collect (freeNamesOf ctxt) ctxt t ty []  
>	collect n ce = Ok . ((n,ce):)
>	show_new_mvs mvs 
>	 = [ show n ++" = "++ showTerm_ ctxt ty ++ "\n\t" ++ show cek  | (n,CE cek ty) <- mvs ]
>	-- show_new_mvs mvs 
>	--  = [ n ++ " = " ++ showTerm_ ctxt ty | nt@(n,CE (M_V _) ty) <- mvs ]

---
chg the mvs to bound vars.

PROBLEM: ensuring that the form of the result is correct...

> assume_not_rule = False

> bind_mvs :: CoercionName -> (Name,Name) 
>						-> [(Name,Ctxt_entry)] -> TypedTerm -> Ctxt_entry
> bind_mvs cn (bv,mv) mvs (TT t ty)
>  | null bs 
>  = error $ "bind_mvs, missing binder: " ++ show dp_type
>  | otherwise
>  = ctr ("After bind: " ++ show cn ++ " " ++ show fo_term) $
>    CE (Coercion fo_term mv_term dom rng assume_not_rule fo_term_def) 
>       dp_type
>    where
>	mv_defs = [ (n, d) | (n, CE (Def d _) ty) <- mvs ]	-- expand def
>	bv_kind = lookup mv mv_defs

>	unsolved = concat $ bindingGroups [ (n, ty) | (n,CE (M_V _) ty) <- mvs ]
>	new_mvs | isJust bv_kind = unsolved
>	        | otherwise      = (mv, Type) : unsolved
>		-- get unsolved metavars
>		-- this may include bv's type if not yet solved by metavars

>	bind_bv :: (BoundName -> Term -> Term -> Term) -> Term -> Term
>	bind_bv bind_fn
>	 | isJust bv_kind = bind_fn (Bound bv) (El $ fromJust bv_kind)
>	 | otherwise      = bind_fn (Bound bv) (El $ MetaVar mv)
>		-- latter, want [m:Type][v:El m] 
>		-- if unsolved src type, then treat as normal mvar
>		-- otherwise, just bind the concrete type.
>		-- add El: assume the BV is in some el-kind for subTYPING.

>	dp_type = foldr (rebind DP) (bind_bv DP ty)            new_mvs
>	fo_term = reduceAppliedAbstrs [] $
>	          foldr (rebind FO) (bind_bv FO (co_to_ap t))  new_mvs 

>	fo_term_def = GlobalDef (CoName cn) fo_term
>		-- represented as definition, to avoid extra work 

>	some_mvs = [ Var $ Plain "?" | _ <- new_mvs ]
>	mv_term = build_ap fo_term_def some_mvs 
>	 -- todo: pre-build str expected by metavars. (but worth it?)

>	(bs,body) = un_DP dp_type
>	end_dp = foldr (uncurry DP) body $ drop (length new_mvs) bs
>		-- do this as a safety check.
>	(dom,rng) = case end_dp of
>					DP _ (El ty) (El t) -> (El ty, El t)
>					other               -> error $ "bind_mvs " ++ show other

---
change a metavar to proper bound variable.
 - push body under binder
 - replace mv with bound var (first_IVar)
 - and add binder for first_IVar.

> rebind bf (n,ty)
>  = bf new_var ty . replaceTerminal mv_to_v0 . shiftUnderOneBinder 
>    where
>	mv_to_v0 (MetaVar m) | n == m = Just first_IVar
>	mv_to_v0 _                    = Nothing
>	new_var = Bound $ Plain $ 'v':tail (show n)


---
co_to_ap
 - removes Co nodes, which have been inserted to prevent more 
   coercions being added during checking.

> co_to_ap :: Term -> Term
> co_to_ap (Co l r) = Ap (co_to_ap l) (co_to_ap r)
> co_to_ap t        = t



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

TO BE DONE.

> showCoercions :: IM
> showCoercions 
>  = do
>	c <- get_M_ 
>	print_M_ $ longRender $ foldr ($$) empty $ reverse $ show_cs c


> show_cs c@(Ctxt es)
>  = [ text "Coercion:" <+> stext n <+> text (if is_rule then "(rule)" else "")
>       $$ nest 2 (    text "=" <+> showTerm c t
>				$$ text ":" <+> showTerm c ty)
>	$$ text ""
>		-- $$ text ("    " ++ show dom ++ " ===> " ++ show rng)
>     | (n, ce@(CE (Coercion t mvt dom rng is_rule _) ty)) <- es ]



