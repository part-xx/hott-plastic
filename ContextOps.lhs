> module ContextOps (
>				assert_is_hyp_or_mv
>				, getReference
>				, CtxtReference(..)
>				, internal_cut
>				, rewritePostContext
>				, update_def_in_Ctxt
>				, univLookup

>				, contextChanges
>				, showCtxtDiffs
>				, ContextDiffs(..)
>				, insertBefore
>				, cutBefore
>				, restrictCoercions
>				) where

Non-primitive Context Operations

#if __GLASGOW_HASKELL__ < 302
> import Trace
#else 
> import IOExts_(trace)
#endif

> import Data.List (mapAccumL)
> import Control.Monad (foldM)
> import Data.Maybe
> import Pretty(vcat,Doc)

--#ifndef __GLASGOW_HASKELL__

> import Hugs_FiniteMap(mapFM)

--#else
--> import FiniteMap(mapFM)
--#endif

> import Base
> import Terms
> import TermOps (replaceTerminal, makeContextVar_Binding, subtrees)
> import TermReduction(compileReductionAtom, compileArbitraryTerm)
> import Context
> import CommandTypes
> import Printing
> import StateMonads

> import SharedSyntax(univ_separator, IdOrNum(..), CoercionName(..))
> import Universes(split_uid)

> import TypeInference(typeCheck, TypedTerm(..))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
References in tactics

 - can be an id (which must be in ctxt)
 - or a relative index of a metavar
 - or nothing, which defaults to the first metavar.


NOTE: if a name, then it could be a ref to ANYTHING
 - should this limit to metavar only?
 - there are cases when metavar-assisted cut on Hyp could be useful...
 - TO BE DECIDED!

> type CtxtReference = Maybe IdOrNum

> getReference :: CtxtReference -> Ctxt -> OkF Name
> getReference Nothing          c = relative_MetaVar 1 c 
> getReference (Just (Right n)) c = relative_MetaVar n c
> getReference (Just (Left n))  c = findCtxt n c >> Ok n

----
look up the n'th metavar.

> relative_MetaVar :: Int -> Ctxt -> OkF Name 
> relative_MetaVar i (Ctxt es) 
>  | i_ < 0
>     = Fail "metavars are indexed from 1."
>  | null $ drop i_ mvs 
>     = Fail $ "not that many metavariables in context " ++ show(i,length mvs)
>  | otherwise
>     = Ok $ mvs !! i_
>  where
>		mvs = [ n | (n, CE (M_V _) _) <- es ]
>		i_ = i - 1


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

internal_cut
 - used in metavars too
 - allow cut term to be in various forms: it only needs the ctxt to check in.

> internal_cut :: Name -> (Ctxt -> OkF Term) -> Ctxt -> OkF Ctxt
> internal_cut n tf c 
>  = do
>		(nc, CE kind ty,post_defs) <- findNameInCtxt n c
>		assert_is_hyp_or_mv kind "Cut"

>		-- trace (showCtxt (Show_Some 20) c ++ "\n") $ Ok ()

>		t1  <- processWithDefs post_defs tf nc
>		-- let t2 = expandSomeDefs post_defs t1

>		let t2 = localiseDefs ((\(PostDefs x) -> x) post_defs) t1
>		-- trace ("IC: "++"\n\t"++show t1++"\n\t"++show t2++"\n") $ Ok ()

>		(TT t3 ty') <- typeCheck nc t2 ty
>				-- don't need typeCheckRawType, since ty is from Ctxt anyway

>		let n_is_def (CE _ ty) = Just $ CE (Def t3 (GlobalDef n t3)) ty
>		let chg_n_to_def = replaceTerminal (chg_to_def n t3) 
>
>		mapOnPostCtxt n_is_def chg_n_to_def n c
>
>	 where
>		chg_to_def n t (ContextVar i) = if i == n then Just t else Nothing
>		chg_to_def n t (MetaVar i)    = if i == n then Just t else Nothing

>		-- chg_to_def n t (MetaVar i)    = if i == n then Just (GlobalDef i t) else Nothing
>		-- WAS: expand metavar in place.

>		chg_to_def n t _              = Nothing

NB chg_to_def - governs def expansion of metavars. CURRENT = expand/replace.
NB if use (= Just GlobalDef), then the metavar name stays like a GlobalDef
	- which could be confusing...
	- we really want to get rid of them when we can !

---
checking CE has right kind, before some operations.

> assert_is_hyp_or_mv kind f_name 
>  = assert_p (is_hyp_or_metavar kind) (f_name ++ " expects for metavar or hyp")



---
Map term-changing function over a CE
 - Needs to be refined. - eg for arities? 	XXXXX check.

Arity: 
 - eg of pre-hyp with a term, then effectively gets removed? 
 - hence need some notion of DEPENDENCIES.

> over_CE :: (Term -> Term) -> Ctxt_entry -> Ctxt_entry
> over_CE f (CE cek ty) = CE (over_CEK f cek) (f ty) 

> over_CEK :: (Term -> Term) -> Ctxt_entry_kind -> Ctxt_entry_kind

> over_CEK f (Hyp a) = Hyp $ f a
> over_CEK f (M_V a) = M_V $ f a

> over_CEK f (Let_ rhs)            = Let_ $ f rhs

> over_CEK f (Def rhs (GlobalDef n _))            
>  = let new = f rhs in
>    Def new (GlobalDef n new)

---

> over_CEK f k@(Bind s) = k
> over_CEK f (ElimOp e a) = ElimOp e (f a)
> over_CEK f (IndFamily cs a i) = IndFamily cs (f a) i

> over_CEK f (FnDef arity rhs (F_Const n _)) 
>  = let new = f rhs in fseq new $ 
>    let new_atom = compileReductionAtom n new in fseq new_atom $
>    FnDef arity new new_atom

---

> over_CEK f c@(Coercion{}) 
>  = Coercion { term    = f $ term c
>             , mv_term = f $ mv_term c 
>             , dom     = f $ dom c 
>             , rng     = f $ rng c 
>             , co_rule = co_rule c 
>             , atom    = f $ atom c 
>             }

---

> over_CEK f i@(IntrosContinuation n ctu)
>  = trace "IntrosContinuation over_CEK - not valid operation" 
>  $ i

-- IntrosContinuation n ctu

-- (\ t -> ctu (f t))

oct02: THIS IS HARD. - the lego metavar problem
would prefer to have ctxt info ready to hand? 
 - stack the updates? 
 - NO - need to know which sub-ctxt is affected
 - in general, it appears safer to leave this as is - any


---

> over_CEK f (Table t) = Table $ mapFM (\_ e -> over_CE f e) t

> over_CEK f k@Mark       = k
> over_CEK f k@ModuleMark = k
> over_CEK f k@FileOpen   = k
> over_CEK f k@FileCalled = k
> over_CEK f k@FileClose  = k

<> x over_CEK f cek                   = error "missing over_CEK case" 


%-------------------------------------------------------------------------------
`update_def_in_Ctxt'
  - replace definition term with other form 
  - intended for reductions. 
  - NOTE: no type checking done on the replacement - so use with care.

> update_def_in_Ctxt :: Name -> Term -> Ctxt -> OkF Ctxt
> update_def_in_Ctxt n d 
>  = mapOnPostCtxt replace_cek  (replaceTerminal replace_def) n
>    where
>		replace_cek (CE (Def _ _) ty) 
>		 = Just $ CE (Def d (GlobalDef n d)) ty
>		replace_cek cek       
>		 = error $ "update_def_in_Ctxt, cek = " ++ show cek

>		replace_def (GlobalDef m _) | m == n = Just $ GlobalDef m d
>		replace_def t                        = Nothing


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

General context modification operation.
 - to modify ctxt entry of name `i' using the given fn
	- it is a Maybe, since we may want to remove it.
	- if return Nothing, then delete the entry. else just replace with new vsn
 - to modify all ctxt after name `i' using the other given fn
 - these fns modify the Ctxt_entry only (hence no name change).

 - uses: implementation of cut, discharge etc.

 - QQ: more abstract is possible? eg iterate ContextVar -> Term?  
	-- or Term -> Term? 
	-- NB Elim arities might change? (eg on discharge parameter)
	-- XXXX may revise in future.

> rewritePostContext :: (Term -> Term) -> Name -> Ctxt -> OkF Ctxt 
> rewritePostContext = mapOnPostCtxt Just
>  -- operation for updating entries after a context change.
>  -- not happy - can't it be generalised - but note cutting is quite different

> mapOnPostCtxt 
>  :: (Ctxt_entry -> Maybe Ctxt_entry)		-- for changing the target.
>     -> (Term -> Term)						-- changing the entries after.
>     -> Name 								-- where to start changes 
>     -> Ctxt 
>     -> OkF Ctxt 

> mapOnPostCtxt cef_nm tef i (Ctxt es)
>  | null foo
>     = fail_with_msg $ "mapOnPostCtxt - name not found: " ++ show i
>  | otherwise 
>     = -- trace ("mapOnPostCtxt " ++ show (i, ces_to_chg) ++ "\n") $
>       Ok $ Ctxt $ [ (n, cef ce) | (n,ce) <- ces_to_chg ] ++ n_ce2 ++ pre_ctxt
>  where
>		cef = over_CE tef
>		cef_ ce = let r = cef ce in trace ("cef_ " ++ show (ce,r) ++ "\n") r
>		ces_to_chg = takeWhile (\(n,ce) -> i /= n) es
>		foo@((_,ce):pre_ctxt) = drop (length ces_to_chg) es
>		n_ce2 = maybe [] (\x -> [(i, x)]) $ cef_nm ce



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Context subtraction

Some ops make context changes which we need to identify. So when we are about
to do an op whose ctxt changes we want, then leave a mark, do the operation,
then pick the ctxt changes up after the mark. 

NB what about intervening cut? 
 - Should really compare the pre-mark contexts for changes like this. 
 - Types could change - SOLELY because of cuts before? 
 - might be valid to take NEW pre-ctxt? 
	- BUT would need to force new cuts ANYWAY? 
 - SWITCH in ctxt - don't allow destructive operations (like cut). 
 - BEAR IN MIND!!! XXXX 

> contextChanges :: CM a -> CM (Maybe (SubCtxt,PreDefs,PostDefs), a)
> contextChanges m
>  = do
>		c_orig <- get_M_ 
>		let mark_id = Plain "mark id"				-- spaces not allowed in id
>		let mark_ty = ContextVar (Plain "mark ty")	-- user can't replicate...
>		let c_mark = add_ctxt mark_id (CE Mark mark_ty) c_orig
>			-- don't bother renaming - we always look for most recent.
>		set_M_ c_mark

>		r <- m
>		c_new <- get_M_
>		changes <- embed $ compareCtxt c_orig mark_id c_new

>		set_M_ c_orig
>		return (changes, r)

---
`compareCtxt'
  - simple trick of getting pre-context in reversed context.
  - leave the entries in context order (so need to reverse the result).
  - also get the PostDefs for the name (from ctxt in proper order!)

******************** TO BE RE-THOUGHT! *******************

> type ContextDiffs = (Ctxt, PreDefs, PostDefs)

> compareCtxt :: Ctxt -> Name -> Ctxt -> OkF (Maybe ContextDiffs)
> compareCtxt orig n (Ctxt es)
>  = do
>		(Ctxt nes, ce, _)  <- findNameInCtxt n (Ctxt $ reverse es)
>		(pre_c@(Ctxt pres) , ce, PostDefs (Ctxt post_defs)) <- findNameInCtxt n (Ctxt es)
>		let pre_defs = find_new_defs orig pre_c 

>		--trace ("PREDEFS = " ++ showCtxt (Show_Some $ length pre_defs) (Ctxt $ pre_defs ++ pres) ++ "\n\n") $ return () 

>		if null (nes ++ pre_defs ++ post_defs) 
>			then return Nothing
>			else return $ Just (Ctxt $ reverse nes, 
>									PreDefs $ Ctxt pre_defs, 
>									PostDefs $ Ctxt post_defs)

> find_new_defs o@(Ctxt os) n@(Ctxt ns) 
>  | map fst os /= map fst ns
>     = error $ "find_new_defs - names differ" 
>					++ showCtxt Show_Everything o ++ "\n------\n"
>					++ showCtxt Show_Everything n ++ "\n------\n"
>  | otherwise
>     = [ n_ce
>        | ((o_n,CE o_cek o_ty), n_ce@(n_n,CE n_d@(Def _ _) n_ty)) <- zip os ns
>		   ,is_hyp_or_metavar o_cek
>		   -- o_n == n_n guaranteed by check above.
>		   ] 

---
`showCtxtDiffs' 

WHY IS THIS SHOWING DOUBLE RESULTS? 

> showCtxtDiffs :: Ctxt -> ContextDiffs -> String
> showCtxtDiffs (Ctxt es) (Ctxt ss, PreDefs (Ctxt ps), PostDefs (Ctxt ds)) 
>  = "Pre-Context\n" 
>    ++ unlines [ showCtxt (Show_Entry n) full_ctxt | (n,_) <- reverse ss ]
>    ++ "Pre-Defs\n"
>    ++ unlines [ showCtxt (Show_Entry n) full_ctxt | (n,_) <- reverse ps ]
>    ++ "Post-Defs\n"
>    ++ unlines [ showCtxt (Show_Entry n) full_ctxt | (n,_) <- reverse ds ]
>    where
>		full_ctxt = Ctxt $ ds ++ ss ++ ps ++ es 		-- NEED REVERSE?
>		-- WAS: full_ctxt = Ctxt $ es ++ ss ++ ds


%-------------------------------------------------------------------------------
`insertBefore' 
  - puts sub-context in before a name.

> insertBefore :: Name -> SubCtxt -> Ctxt -> OkF Ctxt
> insertBefore i (Ctxt chgs) (Ctxt es) 
>  | length rest < 1 
>     = fail_with_msg $ "Name " ++ show i ++ " not found in ctxt."
>  | otherwise
>     = Ok $ Ctxt $ post ++ head rest : chgs ++ tail rest 
>  where
>		(post,rest) = span (\(n,_) -> n /= i) es


> cutBefore :: Name -> PreDefs -> Ctxt -> OkF Ctxt
> cutBefore n (PreDefs (Ctxt ds)) c
>  = foldM 
>		(\c (n,d) -> internal_cut n (\_ -> return d) c) 
>		c 
>		[ (n,d) | (n, CE (Def d _ ) _) <- ds ]

%-------------------------------------------------------------------------------
restrictCoercions

> restrictCoercions :: [Ident] -> Ctxt -> Ctxt
> restrictCoercions ns (Ctxt es)
>  = Ctxt [ e | e@(n,ce) <- es, not_coercion ce || n `elem` co_names ]
>    where
>		co_names = [ CoName $ CN [n] | n <- ns ]
>		not_coercion (CE Coercion{} _) = False
>		not_coercion _                 = True






