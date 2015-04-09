> module IntrosReturn (intros, returnIntros, returnAllIntros) where

Implements the Intros and Return scheme


> import Data.List((\\))
> import Data.Maybe

> import Base
> import Terms
> import SharedSyntax
> import TermOps(replaceTerminal, makeContextVar_Binding)
> import TermReduction(substitute)
> import Context
> import ContextOps

> import Printing
> import Debugging (show_term, ftrace)
> import GlobalOptions (p_Normal)

>  -- TMP (for debugging)
>  -- import TypeInference(typeCheck)
>  -- TMP

> import SimpleTerm
> import SimpleToTerm(translate_db)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
"INTROS"

 - spawn new ctxt with extra hyp in order to derive sub-term (under binder)
 - NB the purpose of intros IS to produce a simplified term...

 - this is the START of multiple context stuff, just that we can't return
   yet from sub-ctxt UNTIL it is resolved....

Pre-refine issue:
   -- NB intros'd term keeps same name throughout.

> intros :: CtxtReference -> IM -> [BoundName] -> IM
> intros ref pre_refine_operation vs
>  = do
>	pre_refine_operation 	-- to be handled properly when I have time.

>	i_n <- use_M_ $ getReference ref 
>	-- print_M_ $ "Intro: " ++ show (n,vs_or_'_')
>
>	intro_vars i_n vs
>	return ()


> intro_vars :: Name -> [BoundName] -> CM [Name]
> intro_vars i_n []
>  = do
>	fail $ "Intros needs some variable names"

> intro_vars i_n vs_or_'_'
>   = do

>	c_orig <- get_M_
>	(c_pre, CE kind ty, _) <- embed $ findNameInCtxt i_n c_orig
>	assert_is_hyp_or_mv kind "Intros"
>          -- keep hold of original context
>          -- get pre-ctxt and type info.
>          -- check 'n' is correct kind and form.


>	(c_decl, vs_bs, new_type) 
>				<- add_hypotheses c_pre vs_or_'_' ty
>	-- want new types or old ones? 
>	-- old ones should be ok - we don't want to rewrite them...

>	let ctu = build_new c_orig (c_pre, i_n, ty) vs_bs

>	let ctu_cek = IntrosContinuation i_n ctu 
>	let ctu_name = mk_ctu_name i_n
>	let c_ctu = add_ctxt ctu_name (CE ctu_cek $ ce_dummy_type "ctu") c_decl
>          -- build continuation and add it.
>          -- nb fix names, since return must look for most recent ctu.

>	let (c_final,n2) = addCtxt_rn (Bound i_n) (CE kind new_type) c_ctu
>	assert_p (n2 == i_n) ("Intro Body renamed: " ++ show (i_n,n2))
>          -- add the body to the ctxt (this name shouldn't change...)

>	-- (CE k1 t1) <- embed $ findCtxt n2 c_final
>	-- print_M_ $ "I2 " ++ nt "k1" (show k1) ++ nt "t1" (show t1) ++ "\n"

>	set_M_ c_final

>	let say_renamed old new 
>		| old /= Bound new = "(" ++ show old ++" --> "++ show new ++ ")"
>		| otherwise        = show new
>	p_Normal $ "Intros for hyp " ++ show i_n ++ ", vars: " 
>			++ show (zipWith say_renamed vs_or_'_' (map fst vs_bs))
>	return $ map fst vs_bs

%---------------------------------------

<> -- NB could add as BoundVar, and rely on db-ctxt interface?

TODO: use CM to pass the monad

> add_hypotheses 
>  :: Ctxt -> [BoundName] -> Type -> CM (Ctxt, [(Name, Binding)], Type)
> add_hypotheses c [] ty
>  = return (c, [], ty)

> add_hypotheses c ns (GlobalDef _ t)
>  = add_hypotheses c ns t
>	-- go through definitions

> add_hypotheses c (Underscore:ns) dp@(DP v@(Bound _) _ _)
>  = add_hypotheses c (v:ns) dp
>	-- underscore overridden by name based on original var

> add_hypotheses c (Underscore:ns) dp@(DP Underscore ty _)
>  = add_hypotheses c (Bound underscore_default_name : ns) dp
>    where
>	underscore_default_name 
>	 | isDP ty              = {-mk-}Plain "f"
>	 | is_Type ty           = {-mk-}Plain "t"
>	 | isEl ty && is_Prf ty = {-mk-}Plain "H"
>        | otherwise            = {-mk-}Plain "x"
>	is_Type Type = True
>	is_Type _    = False
>	is_Prf (Ap (ContextVar prf) l) = prf == nm_prf
>	is_Prf _                       = False
>	nm_prf = Plain "Prf"
>	-- underscore both places, use a default based on type/purpose


> add_hypotheses c (Bound n:ns) dp@(DP v ty t)
>  = do
>	let ((c_decl, new_v), new_cvar)
>	     = (c_v, cvar)
>	       where
>		c_v = addCtxt_rn (Bound n) (CE (Hyp cvar) ty) c
>		cvar = ContextVar $ snd c_v
>	          -- add to ctxt, where it might need renaming.
>	          -- BUT: use the name requested by the user
>	let t' = substitute t new_cvar

>	(c_out, new_vs_bs, ty_out) <- add_hypotheses c_decl ns t'
>	return (c_out, (new_v, (v,ty)) : new_vs_bs, ty_out)

> add_hypotheses c vs ty
>  = fail_with_msg 
>  $ "Intros: more variables than dependent product binders, vars left = "  
>    ++ show vs


---
`build_new'
  - builds a FO ...

> type TermType = (Ctxt, Name, Type)
> type IntroVar_Name = Name


> build_new 
>   :: Ctxt -> TermType -> [(Name,Binding)] -> Term -> OkF Ctxt

> build_new c_orig (c_pre, i_n, dp_ty) vs_bs t
>   = do

>	let bind_one (n,(_,ty)) = makeContextVar_Binding FO n ty 
>	let bound_term = foldr bind_one t vs_bs
>         -- bind all (ContextVar ctxt_v) with a FO binder.
>	  -- Underscore - should not be allowed in FOs.
>	  -- hence we replace (arbitrarily) with the Intros name
>	  -- NB user will prefer these, and it won't affect the type

#if 0
>	let nt m t = "\t{" ++ m ++ " " ++ t ++ "}\n"
>	let st m = nt m . show_term c_orig
>	ftrace ("BN "++ nt "n_bv_ctxtv" (show (n,bv,ctxt_v))
>               ++ st "dp_ty" dp_ty 
>               ++ st "rt" t 
>               ++ nt "RT" ("\n" ++ pretty_crude_print t)
>               ++ st "nt" bound_term 
>               ++ nt "NT" ("\n" ++ pretty_crude_print bound_term) 
>               ++ "\n") $ return ()
#endif

>	-- SUPERFLUOUS - since internal_cut does a type-check anyway
>	-- (TT bound_term2 ty_bound_term) <- typeCheck pre_c bound_term dp_ty

>	c_new <- customise_error  
>		$ internal_cut i_n (\c -> return bound_term) c_orig
>	return c_new
>   where
>	customise_error = change_msg ("rebuild fail: " ++)



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
"RETURN"

> returnIntros :: Maybe Name -> IM 
> returnIntros Nothing
>   = -- catch_M_ (\m -> p_Normal $ m ++ "\nDoing nothing.") $
>   do
>	nm <- use_M_ $ \c -> most_recent_continuation c
>	returnIntros (Just nm)

> returnIntros (Just n)
>   = do
>	c <- get_M_
>	c' <- embed $ return_intros_ n c
>	p_Normal $ "Return succeeded on " ++ show n
>	set_M_ c'

> returnAllIntros :: IM
> returnAllIntros 
>   = do
>	cs <- use_M_ (Ok . ctus)
>	if null cs
>        then p_Normal "No Intros to undo"
>        else do
>            while_M_ returnIntros (map (const Nothing) cs)
>            cs2 <- use_M_ (Ok . ctus)
>            if null cs2
>              then p_Normal $ "All Intros undone"
>              else p_Normal $ 
>                          "Intros undone up to " ++ show (head cs2) ++ "\n"
>                          ++ "Remaining intros: (" 
>                          ++ unwords (map show cs2) ++ ")\n" 
>   where
>	ctus (Ctxt es) = [ id | (_, CE (IntrosContinuation id _) _) <- es ]

> return_intros_ :: Name -> Ctxt -> OkF Ctxt
> return_intros_ n c 
>   = do
>	(c_pre, CE kind ty, post_defs) <- findNameInCtxt n c
>	rhs <- get_def kind
>	-- WF checked when term is cut in new continuation, 
>	-- BUT this seems a bit disjointed !

>	ctu <- get_continuation n c_pre

#if 0
>	let rl = localiseDefs (case post_defs of PostDefs x -> x) rhs
>	ftrace ("LOC term = " ++ showTerm_ c_pre rl ++ "\n") $ return ()
#endif
>	let rx = expandSomeDefs post_defs rhs
>	-- ftrace ("EXP term = " ++ showTerm_ c_pre rx ++ "\n") $ return ()

>	c' <- ctu $ rx        -- or rl
>	return c'
>   where
>	get_def (Def rhs _) = Ok rhs
>	get_def _       = Fail $ "return expects a definition"

---

---

> most_recent_continuation :: Ctxt -> OkF Name
> most_recent_continuation (Ctxt es) 
>  | null cs   = Fail "No continuations in this context."
>  | otherwise = Ok $ head cs
>   where
>	cs = [ id | (_, CE (IntrosContinuation id _) _) <- es ]

> get_continuation n c 
>  = change_msg (++ "Couldn't find continuation") $
>    do
>    (CE (IntrosContinuation n2 ctu) _) <- findCtxt (mk_ctu_name n) c
>    assert_p (n == n2) ("Continuation name problem: " ++ show (n,n2))
>    return ctu

---
`mk_ctu_name'
  - build the continuation name as an impossible id. (ie, contains spaces)
  - this is replicated somewhere else? 

> mk_ctu_name n = Plain $ "ctu " ++ show n

