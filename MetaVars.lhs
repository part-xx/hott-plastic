> module MetaVars (
>				test_CTC
>				, simple_MetaVars
>				, refine_MetaVars
>				, definition_MetaVars
>				) where

Top level calls for Constraint Type Inference (old MetaVars interface).


> import BaseClasses(OkF(..))
> import BaseClasses(Fallible(..), Embeddable(..), Permutable(..))
> import Base(Forceable(..))

> import StateMonads(use_M_, get_M_, set_M_)
> import PrettyAux(longRender, stext)
> import Pretty(Doc, vcat, text, (<+>))

> import DebugOptions(traceOnDebugFlag)

> import Terms
> import TermOps(subtrees) --, replaceTerminal, terminals)
> import SharedSyntax(Name(..))

> import Context hiding (IM)
> import ContextOps(internal_cut)
> import SimpleToTerm(translate_db, SimpleTerm, SimpleType(..))

> import Printing(showTerm, showTerm_, showTermAndType, showCtxt, CtxtShow(..))
> import Reduction(dwhnf, flatten, unflatten, FlatTerm(..))

> import qualified TypeInference(typeCheck, inferType)

> import ConstraintTypeInf(inferType)

#ifndef __HASKELL98__
#define fmap map
#endif

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Tracing etc

> it_where :: String -> a -> a
> it_where s = tr ("C_IT case = " ++ s ++ "\n")

> -- tr s = traceOnDebugFlag "" (s `fseq` s)
> tr s = traceOnDebugFlag "METAVAR_TRACING" (s `fseq` s)

> mtr s = tr (s ++ "\n") $ return ()

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Adding subst info to ctxt.
  - nb some duplic with add_if_used...
  - WHAT ABOUT DEPENDENCIES? 

  - ALSO! *** localiseDefs *** ...

> add_or_cut :: Name -> Ctxt_entry -> Ctxt -> OkF Ctxt

> add_or_cut n ce@(CE (M_V _) ty) c 
>  = it_where ("add_or_cut M_V " ++ show n ++ " " ++ show ty) $
>    do
>		c' <- embed $ addCtxt (Bound n) ce c		-- nb add_if_unused
>		-- then fail_with_msg $ "add_or_cut: name not free " ++ show (n,n')
>		return c'

> add_or_cut n (CE (Def d _) ty) c 
#if 0
>	-- can't do this yet - substs are not propagated. 
>	-- should be simple chg, but need to track it through.
>  = return c		-- not keeping definitions.
#else
>  = do
>		c' <- case (findCtxt n c) of
>				Fail _ -> add_or_cut n (CE (M_V $ MetaVar n) ty) c
>				Ok _   -> return c
>		it_where msg $ return ()
>		internal_cut n (const $ Ok d) c'
>    where
>		msg = "add_or_cut DEF "++ show n ++" "++show d++" "++show ty
#endif

%-------------------------------------------------------------------------------
External Fns in current metavar interface.

> simple_MetaVars :: FreeNameSupply -> SimpleTerm -> CM (Maybe String,Term)
> simple_MetaVars fns t
>  = do
>		t0 <- use_M_ $ \c -> translate_db c t
>		if (doesn't_contain_metavars t0)
>			then return (Nothing, t0)
>			else simple_MetaVars_ fns t0

> simple_MetaVars_ fns t0
>  = do
>		let add_ n ce (c, ns) = add_or_cut n ce c >>= \c' -> return (c', n : ns)
>		((c',ns), TT t1 _) <- use_M_ $ \c -> inferType add_ fns c t0 Nothing (c,[])
>		set_M_ c'
>		let show_ n = showCtxt (Show_Entry n) c'
>		let text = unlines $ map show_ $ reverse ns
>		return (if null ns then Nothing else Just text, t1)

---

Horrible cases for decoding and seeing whether (opt) raw type needs to be
processed.

> definition_MetaVars
>  :: FreeNameSupply -> SimpleTerm -> Maybe SimpleType -> CM (Term,Maybe Type)
> definition_MetaVars fns t Nothing
>  = do
>		t0 <- use_M_ $ \c -> translate_db c t
>		if (doesn't_contain_metavars t0)
>			then return (t0, Nothing)
>			else definition_MetaVars_ fns t0 Nothing

> definition_MetaVars fns t (Just ety)
>  = do
>		t0 <- use_M_ $ \c -> translate_db c t
>		ty0 <- use_M_ $ \c -> translate_db c ety
>		if (doesn't_contain_metavars t0)
>			then if (doesn't_contain_metavars ty0)
>					then return (t0, Just ty0)
>					else do
>							plainTerm_definition_MetaVars_ fns t0 ty0
>			else definition_MetaVars_ fns t0 (Just ty0)

---
Get type of T via TypeInference. inferType
send it through d_ as CAST
remove the cast.

> plainTerm_definition_MetaVars_ fns t ty
>  = do
>		(TT t1 ty_t1) <- use_M_ $ \c -> TypeInference.inferType c t
>		let fake_t1 = cAST t1 ty_t1
> 		(t2, ty_t2) <- definition_MetaVars_ fns fake_t1 (Just ty)
>		case (flatten t2) of
>			E (Ext (HardCast t _)) as 
>			    -> return (unflatten $ E t as, ty_t2)
>			_   -> error $ "definition_MetaVars_ CAST" ++ show t2


---


> definition_MetaVars_ fns t0 ty0
>  = do
>		c0 <- get_M_ 
>		(c', TT t1 ty_t1) <- embed $ inferType add_or_cut fns c0 t0 ty0 c0
>		set_M_ c'
>		return (t1, maybe Nothing (\_ -> Just ty_t1) ty0)

NOTE: silly to have type dropped from def_mvs? ie, see last line nonsense

---

PROCESS WHETHER HAVE '?' or not - since might be solving some metavars in
the process.

MIGHT WANT TO TRY TI.typeCheck as quick route? 

> refine_MetaVars :: FreeNameSupply -> Term -> Type -> CM Term
> refine_MetaVars fns t ety
>  = refine_MetaVars_ fns t ety

> refine_MetaVars_ fns t ety
>  = do
>		c0 <- get_M_ 
>		(c', TT t1 ty_t1) <- embed $ inferType add_or_cut fns c0 t (Just ety) c0
>		set_M_ c'
>		mtr $ "NEW CTXT AFTER REFINE = " ++ showCtxt (Show_Some 10) c' ++ "\n"
>		return t1


ADD AND RECURSE! 
TRY HACK: if ty_ch fails and kind is DP, then try adding metavar.
---

> doesn't_contain_metavars :: Term -> Bool
> doesn't_contain_metavars
>  = null . subtrees is_metavar
>    where
>		is_metavar (Var (Plain ('?':_))) = True
>		is_metavar (MetaVar _)           = True
>		is_metavar _                     = False


%-------------------------------------------------------------------------------
test function - showing results as string.

> test_CTC :: Ctxt -> Term -> Maybe Type -> OkF String
> test_CTC c t mty
>  = do
>		(ss, tt) <- inferType collect_ce (freeNamesOf c) c t mty []
>		return $ longRender $ vcat $ process_and_show c ss tt
>    where
>		collect_ce n ce nces = Ok $ (n,Right ce) : nces

---

> process_and_show :: Ctxt -> [(Name, Either () Ctxt_entry)] -> TypedTerm -> [Doc]
> process_and_show (Ctxt es) ss (TT t ty)
>  = text "\nRESULTS:" 
>     :  [ stext n <+> stext tt | (n,tt) <- ss ] 
>     ++ [showTerm c2 t, showTerm c2 ty]
>     ++ [text "", text ""]
>    where
>		decls = [ (n, ce) | (n, Right ce@(CE (M_V _) _)) <- ss ]
>		c2 = Ctxt $ foldr (:) es $ order decls

TO DO - hijack ctxt showing code .

> order = id


%-------------------------------------------------------------------------------
Local test values.

#if 0
> [vA,va,vf,vg,vh,vj] = map ContextVar $ words "A a f g h j"
> [q,qx,qy] = map Var $ words "? ?x ?y"
> [v1,v2,v3] = map IVar [1..3]

> c0 = Ctxt [
>			("A", CE Hyp Type)
>			,("a", CE Hyp $ El vA)
>			,("f", CE Hyp $ DP "_" (El vA) (El vA))
>			,("g", CE Hyp $ DP "_" (El vA) (DP "_" (El vA) (El vA))) 
>			,("h", CE Hyp $ DP "T" Type (DP "_" (El v1) (El v2)))
>			]

---

> run mty t = putStr $ elimOk ("FAIL:\n"++) id $ new_it c0 t mty

> a = va
> f = Ap vf
> g = Ap . (Ap vg) 
> h = Ap . (Ap vh)

> t1 = f $ f $ f $ f a

#endif

