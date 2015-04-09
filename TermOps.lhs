> module TermOps (
>			replaceTerminal,
>			un_DP,
>			terminals,
>			is_terminal,
>			subtrees,
>			namesIn,
>			bindingGroups,

>			addDeBruijnIndices,
>			makeContextVar_Binding,
>			isIVarUsed,
>			isFirstIVarUsed,
>			applyToDummy

>			, db_start				-- from TermReduction
>			, shiftUnderSomeBinders
>		) where


#if __GLASGOW_HASKELL__ < 302
> import Trace
#else
> import IOExts_(trace)
#endif
> import Data.Maybe(catMaybes)
> import Data.List(partition, intersect, (\\))
> import Control.Monad

> -- import DebugOptions(traceOnDebugFlag)
> import Base
> import Terms
> import Terms(isAtom)		-- to ensure name is in .hi file...
> import SharedSyntax

> import TermReduction(ivar_caf, db_start, test_ivars, BindingDepth(..))
> import TermReduction(
>				shiftUnderSomeBinders
>				,shiftUnderOneBinder
>				,shiftOverSomeBinders
>				,shiftOverOneBinder)



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Adding deBruijn indices

 - Replaces bound names by deBruin indices, starting from `db_start'. 
 - This function should only be used on partially-translated terms.
 - Use `makeContextVar_Binding' if you want to re-bind a context var.


%---------------------------------------
Abstraction for Name Coding

Keeps table of bound name and method to construct the final term.

> newtype NameList = NL [(BoundName, DB_Index -> Term)]
> empty_NameList = NL []
> not_in_NameList i (NL ns) = Bound i `notElem` map fst ns

---

Given plain name, see if it was bound anywhere.

> dB_Term :: Name -> NameList -> Term
> dB_Term n (NL ns) 
>  = elimOk error id $ 
>	 case [ f i | ((m,f),i) <- zip ns [db_start ..], Bound n == m ] of
>		[]    -> Fail $ "dB_term: didn't find " ++ show n ++ " in list."
>		(t:_) -> Ok t


---

---
Adding to NameLists


> add_Binder :: BoundName -> NameList -> NameList
> add_Binder n (NL ns) = NL ((n, ivar_caf) : ns)

> add_Let    :: Name -> Term -> NameList -> NameList
> add_Let  n t (NL ns) 
>  = NL ((Bound n, \i -> LocalDef i (shiftUnderSomeBinders (i+1) t)) : ns) 

NOTE: Lets are not fully bound names, since they can't be Underscore



%-------------------------------------------------------------------------------
Actual Substitution

> addDeBruijnIndices :: Term -> Term
> addDeBruijnIndices t
>  = runRM (adb t) empty_NameList

---
Monad to help the process.

NB needs to have poly var, if going to make monadic...
QQ: isn't this a compromise on expression of intention? 

> type DM a = RM NameList a

---

> index_valid :: BindingDepth -> DM ()
> index_valid i
>  = do
>		NL ns <- getStateRM
>		if (i >= db_start && (i - db_start) < length ns) 
>			then return ()
>			else error $ "Invalid DB index, " ++ show i ++ "\n" ++ show ns

---
The actual function

> adb :: Term -> DM Term

First, the simple cases.

> adb t@(Kind)          = found_Kind "addDeBruijnIndices"
> adb t@(Type)          = return t		-- no change to these cases.

> adb t@(Elim _ _)      = return t
> adb t@(Const _ _)     = return t
> adb t@(F_Const _ _)   = return t
> adb t@(F_Elim _)      = error $ "F_Elim in adb, " ++ show t
> adb t@(Closed _ _)    = error $ "Closed in adb, " ++ show t

> adb t@(GlobalDef _ _) = return t
> adb t@(ContextVar _)  = return t

> adb t@(MetaVar _)     = return t

> adb t@(IVar i)
>  = do
>		index_valid i
>		return t

<>  = error "shouldn't be adding deBruin indices if already done so. (IVar)"

> adb t@(LocalDef _ _)
>  = error "shouldn't be adding deBruin indices if already done so. (Let)"


Now the recursive cases.

> adb (El t)
>  = do
>		t' <- adb t
>		return (El t')

> adb (Ap t1 t2)   = adb_pair Ap t1 t2

> adb (Ext (SoftCast t1 t2)) 
>  = adb_pair ((Ext.).SoftCast) t1 t2
> adb (Ext (HardCast t1 t2))
>  = do
>		t2' <- adb t2			-- don't visit e.
>		return (Ext (HardCast t1 t2'))

> adb (Ext e) 
>  = fmap Ext $ mapM_Extension adb e


Now the cases that do the work.

> adb t@(Var (Plain ('?':_))) = return t
>	-- leave ?vars in place - metavar routines will remove them later.

> adb (Var v)
>  = useStateRM $ dB_Term v

> adb (DP v ty t) = adb_binder DP v ty t
> adb (FO v ty t) = adb_binder FO v ty t

> adb (Let v t1 t2)
>  = do
>		t1' <- adb t1
>		t2' <- chgStateRM (add_Let v t1') $ adb t2
>		return (Let v t1' t2')


And an error condition, just in case.
Could let the compiler (ghc) do the work (-fwarn-incomplete-patterns)
But here, want to assert that certain constrs are not handled 

> adb t
>  = unknown_constructor "adb" t

---
Generic function for branches

> adb_pair f t1 t2
>  = do
>		t1' <- adb t1
>		t2' <- adb t2
>		return (f t1' t2')

---
Generic function for all (lambda) binders.

> adb_binder :: (BoundName -> Term -> Term -> Term) 
>								-> BoundName -> Term -> Term -> DM Term
> adb_binder f v ty t
>  = do
>		ty' <- adb ty
>		t' <- chgStateRM (add_Binder v) $ adb t
>		return (f v ty' t')

NB var doesn't bind in its type. 



%-------------------------------------------------------------------------------
`makeContextVar_Binding'
  - special case of above, where we are about to re-bind a SINGLE id, which
	was in the Context, throughout a term.

  - main difference is that it doesn't mind IVars being here, and that 
    ignores binders with same name - to avoid false capture.
  - and it only operates on a single id.

> makeContextVar_Binding 
>  :: (BoundName -> Term -> Term -> Term) -> Name -> Type -> Term -> Term
> makeContextVar_Binding bind_f cvar ty t
>  = -- trace (force $ "in makeContextVar_Binding (" ++ i ++ "): \n") $
>    -- trace (force $ (show t) ++ "\n\n") $ 
>    flip runRM empty_NameList $
>    adb_cvar cvar (bind_f (Bound cvar) ty $ shiftUnderOneBinder t)

---
`adb_cvar'
  - does the work.
  - explicit pass the (constant) cvar id to highlight this factor, 
	and otherwise would need a new monad... (so being lazy here)

> adb_cvar :: Name -> Term -> DM Term

First, the simple cases.

> adb_cvar cvar t@(Kind)          = found_Kind "makeContextVar_Binding"
> adb_cvar cvar t@(Type)          = return t

> adb_cvar cvar t@(IVar _)        = return t

> adb_cvar cvar t@(Elim _ _)      = return t
> adb_cvar cvar t@(F_Elim _)      = return t

> adb_cvar cvar t@(Const _ _)     = return t
> adb_cvar cvar t@(F_Const _ _)   = return t
> adb_cvar cvar t@(Closed o _)    = adb_cvar cvar o

>	-- const can't contain a ContextVar which needs to be bound outside.
>	-- unless in the types of course... 
>   -- BUT since we don't do general discharge, it should NOT be a problem.
>	-- check for defs-only in post-ctxt will avoid this case in Return Intros...
>	-- should KILL (ie, error ()) these terms? and require that caller fixes?

> adb_cvar cvar t@(LocalDef i d)  
>  = do
>		d' <- adb_cvar cvar d		-- should just do lookup here? 
>		return $ LocalDef i d'		-- cf case in type inference...

> adb_cvar cvar (El t)
>  = do
>		t' <- adb_cvar cvar t
>		return (El t')

> adb_cvar cvar (Ap t1 t2)   = adb_cvar_pair cvar Ap t1 t2
> adb_cvar cvar (Co fn arg)  = adb_cvar_pair cvar Co fn arg

> adb_cvar cvar (Ext (SoftCast t1 t2))
>  = adb_cvar_pair cvar ((Ext.).SoftCast) t1 t2
> adb_cvar cvar (Ext (HardCast t1 t2)) 
>  = do
>		t2' <- adb_cvar cvar t2			-- don't visit e.
>		return (Ext (HardCast t1 t2'))


Now the cases that do the work.

> adb_cvar cvar t@(ContextVar v)
>  | cvar /= v = return t
>  | otherwise = useStateRM $ dB_Term v

> adb_cvar cvar t@(MetaVar v)
>  | cvar /= v = return t
>  | otherwise = useStateRM $ dB_Term v


> adb_cvar cvar t@(GlobalDef v _) 
>  | cvar /= v = return t
>  | otherwise = useStateRM $ dB_Term v
>	-- shouldn't contain re-bound var
>	-- any problems handled by special treatment of defs in Return Intros...
>	-- should KILL (ie, error ()) these terms? and require that caller fixes?


> adb_cvar cvar t@(Var (Plain ('?':_))) = return t 
>	-- leave ?vars in place - metavar routines will remove them later.
>   -- XXXX this (and similar above) might be REDUNDANT?? 

> adb_cvar cvar (Var v)
>  = error $ "adb_cvar found Var (" ++ show v ++ ") - should not happen."

> adb_cvar cvar (DP v ty t) = adb_binder_cvar cvar DP v ty t
> adb_cvar cvar (FO v ty t) = adb_binder_cvar cvar FO v ty t

> adb_cvar cvar (Let v t1 t2)
>  = do
>		t1' <- adb_cvar cvar t1
>		t2' <- chgStateRM (add_Let v t1') $ adb_cvar cvar t2
>		return (Let v t1' t2')


And an error condition, just in case.
Could let the compiler (ghc) do the work (-fwarn-incomplete-patterns)
But here, want to assert that certain constrs are not handled 

> adb_cvar cvar t
>  = unknown_constructor ("adb_cvar (" ++ show cvar ++ ")") t

---
Generic function for branches

> adb_cvar_pair cvar f t1 t2
>  = do
>		t1' <- adb_cvar cvar t1
>		t2' <- adb_cvar cvar t2
>		return (f t1' t2')

---
`adb_binder_cvar'
  - handles case for binders
  - only add a binding name to the NameList (state) when it is the FIRST 
	(ie, binding) occurrence of the cvar 
  - but keep the original var name. 

> adb_binder_cvar
>  :: Name -> (BoundName -> Term -> Term -> Term) 
>                                  -> BoundName -> Term -> Term -> DM Term
> adb_binder_cvar cvar f v ty t
>  = do
>		ns  <- getStateRM
>		let v2 | Bound cvar == v && cvar `not_in_NameList` ns = v	-- first occ
>		       | otherwise                                    = Underscore
>		ty' <- adb_cvar cvar ty
>		t'  <- chgStateRM (add_Binder v2) $ adb_cvar cvar t
>		return (f v ty' t')

NB this should also do GlobalDefs to Lets!!!

%-------------------
XXXXX Don't understand this stuff about first occ.!!  check it again!

outer cvar being made a DB index. 

ok, it is about HIDING a potential re-binding occ of cvar. 

doesn't look right though. 
%-------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
`isFirstIVarUsed' checks whether (IVar db_start) is unused in E.

> isFirstIVarUsed :: Term -> Bool
> isFirstIVarUsed = isIVarUsed db_start

> isIVarUsed :: DB_Index -> Term -> Bool
> isIVarUsed i = test_ivars (i ==)




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
`replaceTerminal' recurses through a term, replacing terminals which don't 
produce `Nothing' from the test function by the Just value. 

 - NB this could be confusing - we could construct rt_maybe outside and avoid
   the extra indirection!
 - although, it does show abstraction, in that we choose to replace or not...

 - replace fn counts binding depth, so will allow replacement of free (ie
   non-bound) DB terms. 

 -  IMPORTANT with binding depth, this makes it very close to substitution 
	 - in fact, replaceTerminal generalises it. 

> replaceTerminal :: (Term -> Maybe Term) -> Term -> Term
> replaceTerminal tf
>  = rt_ db_start (rt_maybe tf)

---
`rt_maybe'
  - if function says replace term(inal), then do so, else return original.

> rt_maybe tf d t
>  = case (tf $ shiftOverSomeBinders d t) of 
>		Just t' -> shiftUnderSomeBinders d t'
>		Nothing -> t


---

> rt_ :: BindingDepth -> (BindingDepth -> Term -> Term) -> Term -> Term

> rt_ d tf t@(Kind)          
>  = case tf d t of			-- only allow replacement of Kind with Kind.
>		Kind -> t
>		t2   -> error $ "replaceTerminal, can't replace Kind with " ++ show t2
> -- rt_ d tf t@(Kind)          = found_Kind "replaceTerminal"

> rt_ d tf t@(Type)          = tf d t

> rt_ d tf t@(Elim _ _)      = tf d t
> rt_ d tf t@(F_Elim _)      = tf d t
> rt_ d tf t@(Const _ _)     = tf d t
> rt_ d tf t@(F_Const _ _)   = tf d t
> rt_ d tf t@(Closed _ _)    = tf d t

> rt_ d tf t@(GlobalDef _ _) = tf d t
> rt_ d tf t@(ContextVar _)  = tf d t
> rt_ d tf t@(MetaVar _)     = tf d t

> rt_ d tf t@(Var _)         = tf d t	
>	-- should these remain? maybe error() it.
>	-- but, replaceTerminal is sometimes used to remove them, so leave as is.

> rt_ d tf t@(IVar i)
>  = case compare i d of
>		LT -> t				-- bound var, leave alone.
>		_  -> tf d t

> rt_ d tf t@(LocalDef i _)
>  = case compare i d of
>		LT -> t				-- bound var, leave alone.
>		_  -> tf d t

UNSURE: 
	what to do about renaming of IVars etc before calling tf. 
	TESTS needed


The recursive cases - just propagate and apply fn to result. 

> rt_ d tf (El t)            = El (rt_ d tf t)
> rt_ d tf (Ap l r)          = Ap (rt_ d tf l) (rt_ d tf r)
> rt_ d tf (Co f a)          = Co (rt_ d tf f) (rt_ d tf a)

> rt_ d tf (DP v ty t)       = DP v  (rt_ d tf ty) (rt_ (d+1) tf t)
> rt_ d tf (FO v ty t)       = FO v  (rt_ d tf ty) (rt_ (d+1) tf t)
> rt_ d tf (Let v t1 t2)     = Let v (rt_ d tf t1) (rt_ (d+1) tf t2)

> rt_ d tf (Ext (SoftCast e t)) = Ext $ SoftCast (rt_ d tf e) (rt_ d tf t)
> rt_ d tf (Ext (HardCast e t)) = Ext $ HardCast (rt_ d tf e) (rt_ d tf t)		-- DEBATE

<> rt_ _ tf t 
<>  = unknown_constructor ("rt_") t

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
`terminals'
  - produce list of terminal nodes.

> terminals :: Term -> [Term]
> terminals = subtrees is_terminal


---
Common pattern.

> namesIn :: Term -> [Name]
> namesIn = catMaybes . map nameFromTerm . terminals



%-------------------------------------------------------------------------------
`is_terminal'

> is_terminal (IVar _)        = True
> is_terminal (LocalDef _ _)  = True

> is_terminal (Type)          = True

> is_terminal (Const _ _)     = True
> is_terminal (F_Const _ _)   = True
> is_terminal (Closed _ _)    = True
> is_terminal (Elim _ _)      = True
> is_terminal (F_Elim _)      = True

> is_terminal (GlobalDef _ _) = True
> is_terminal (ContextVar _)  = True
> is_terminal (MetaVar _)     = True
> is_terminal (Var _)         = True
> is_terminal (Kind)          = found_Kind "is_terminal"

> is_terminal (El _)          = False
> is_terminal (Ap _ _)        = False
> is_terminal (DP _ _ _)      = False
> is_terminal (FO _ _ _)      = False
> is_terminal (Co _ _)        = False
> is_terminal (Ext (SoftCast {})) = False
> is_terminal (Ext (HardCast _ _)) = False
> is_terminal (Let _ _ _)     = False


%-------------------------------------------------------------------------------
`subtrees'
  - recurse through trees, returning subtrees which satisfy the predicate
  - this is both a filtering and a flattening operation

  - NB what about bound vars. Lead to notion of SLICE of a term?
       eg Gtree with BV info at node; would need post elim. of FVs. 

> subtrees :: (Term -> Bool) -> Term -> [Term]

> subtrees p x@(El l)        = pick p x $ subtrees p l 
> subtrees p x@(Ap l r)      = pick p x $ subtrees p l   ++ subtrees p r
> subtrees p x@(DP m ty t)   = pick p x $ subtrees p ty  ++ subtrees p t
> subtrees p x@(FO m ty t)   = pick p x $ subtrees p ty  ++ subtrees p t
> subtrees p x@(Co f a)      = pick p x $ subtrees p f   ++ subtrees p a
> subtrees p x@(Let m rhs t) = pick p x $ subtrees p rhs ++ subtrees p t

<> subtrees p x@(Ext (SoftCast e t)) = pick p x $ subtrees p e   ++ subtrees p t
<> subtrees p x@(Ext (HardCast e t)) = pick p x $ subtrees p e   ++ subtrees p t

> subtrees p x@(Ext e) = fold_Extension (pick p x.concat) (subtrees p) e

--- 
anything else should be a terminal 
 - give explicit cases for protection.

> subtrees p x@(IVar _)        = pick p x []	-- debatable, since DB terms
> subtrees p x@(LocalDef _ _)  = pick p x []	-- might lose context... 
>												-- IDEA: chg these back to Var?

> subtrees p x@(Type)          = pick p x []

> subtrees p x@(Const _ _)     = pick p x []
> subtrees p x@(F_Const _ _)   = pick p x []
> subtrees p x@(Closed _ _)    = pick p x []
> subtrees p x@(Elim _ _)      = pick p x []
> subtrees p x@(F_Elim _)      = pick p x []

> subtrees p x@(GlobalDef _ _) = pick p x []
> subtrees p x@(ContextVar _)  = pick p x []
> subtrees p x@(MetaVar _)     = pick p x []
> subtrees p x@(Var _)         = pick p x []	
> subtrees p x@(Kind)          = pick p x [] -- found_Kind "subtrees"

ghc will complain about missing constrs.

---

> pick :: (Term -> Bool) -> Term -> [Term] -> [Term]
> pick p t e = if p t then [t] else e

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Un-binding terms.

`un_DP' reduces a term to a list of DP bindings and the remaining body. It
converts DB_Indices to their original (Var n) equivalent.

It DOESN'T do reduction first. 

DISABLE replacement with Var. - hard to do unless we don't mind Var in shift.
(was sanity check measure - think better to rethink how binders done)

> un_DP :: Term -> ([Binding], Term)
> un_DP (DP n ty t)
>  = ((n,ty) : bs, r)
>    where
>		(bs, r) = un_DP $ un_db n t 
>		un_db n t = t

<>		un_db n t = replaceTerminal ivar0_to_Var t
<>						-- need Var, in order to repl. with DB_Index
<>		ivar0_to_Var (IVar i) | i == db_start = Just $ Var n
<>		ivar0_to_Var t                        = Nothing 
<>		

> un_DP t
>  = ([], t)


---
`applyToDummy' modifies term as if it was reduced by

([y:?][x:?]y x) T => [x:?]T x 

And then the binder of x was REMOVED. It is used in conversion, for eta.

> applyToDummy :: Term -> Term
> applyToDummy t = Ap (shiftUnderOneBinder t) (ivar_caf db_start)





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
`bindingGroups'
  - divide a list of named terms into groups, the i'th only depending on
	the ones before it.
  - the groups increase in dependence.
  - can use for types or terms.

> bindingGroups :: [(Name,Term)] -> [[(Name,Term)]]
> bindingGroups nts
>  = separate_groups tagged
>    where
>		names = map fst nts
>		tagged = [ (names `intersect` namesIn t, nt) | nt@(_,t) <- nts ]

> separate_groups [] = []
> separate_groups nts
>  = map snd no_deps 
>			: separate_groups [ (ns \\ no_dep_nms, nt) | (ns, nt) <- other ]
>    where
>		(no_deps,other) = partition (null.fst) nts
>		no_dep_nms = map (fst.snd) no_deps


