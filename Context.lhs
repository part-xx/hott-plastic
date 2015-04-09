> module Context (module Context, Name) where

Types etc for context.

#if __GLASGOW_HASKELL__ < 302
> import Trace
#else 
> import IOExts_(trace)
#endif

#ifndef __HASKELL98__
#define fmap map
#endif

> import Data.Char(isDigit)
> import Data.Maybe(listToMaybe, catMaybes,isNothing)
> import Data.List(tails)

--#ifndef __GLASGOW_HASKELL__

> import Hugs_FiniteMap(FiniteMap, listToFM, lookupFM, addToFM_C)

--#else
--> import FiniteMap(FiniteMap, listToFM, lookupFM, addToFM_C)
--#endif


> import Base
> import Terms
> import TermOps
> import SharedSyntax(unbind, CoercionName(..), split_uid, univ_separator)

#ifndef __HASKELL98__
#define fmap map
#endif

#if 0
<> UserSyntax import 		-- FOR LATER.
#else
> data UserSyntaxInfo = USI () deriving Show 		-- NYI.
#endif


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
IDEAS:

Plan to separate - need something which represents ADT of context.
Also want to enforce the abstractness of the underlying type 

[oct02]
  essential to make this an ADT.

%-------------------------------------------------------------------------------




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Context

 -- ALL context entities have type which we don't want to infer
 -- corollary - need to change on discharge etc. 
 -- INVARIANT: things in context are type-checked already.

> type ID_CE = (Name, Ctxt_entry) 
>		-- local synonym for the basic element of contexts.

> newtype Ctxt 
>  = Ctxt [ID_CE]              -- context is a just a lookup-table.
>    deriving Show


---
synonym representing "not quite a full context".

> type SubCtxt = Ctxt

---

> emptyCtxt = Ctxt []


---
Normal Find.
 - special case for backticks.
 - Maybe on aux reqd. by table lookup (at present)

> findCtxt :: Name -> Ctxt -> OkF Ctxt_entry
> findCtxt (Backticks n) c
>  = findCtxt (Plain n) c
> findCtxt i (Ctxt es) 
>  = maybeToOkF ("'" ++ show i ++ "' not found in context")
>  $ ctxt_lookup i es

> ctxt_lookup :: Name -> [ID_CE] -> Maybe Ctxt_entry
> ctxt_lookup i [] 
>  = Nothing
> ctxt_lookup i ((_,CE (Table t) _):_)
>  = lookupFM t i
> ctxt_lookup i ((n,ce):id_ces)
>  | i == n    = Just ce
>  | otherwise = ctxt_lookup i id_ces


---
special universe (dynamic id) lookup

> univLookup :: Name -> Ctxt -> OkF Term
> univLookup n c
>  = do
>	(p,i) <- split_uid n
>	(CE (Dynamic gf) _) <- findCtxt (Plain $ p ++ [univ_separator]) c
>	gf p i




---
Find name and entry of a DeBruijn indexed variable (lambda or let)

NB we need both name and type.

MAYBE can turn the check off in actual use.

> findCtxt_DB :: DB_Index -> Ctxt -> (Name, Type)
> findCtxt_DB i c = case findCtxt_DB_aux i c of (n,_,ty) -> (n,ty)

> findCtxt_DB_let :: DB_Index -> Ctxt -> (Term, Type)
> findCtxt_DB_let i c = case findCtxt_DB_aux i c of (_,t,ty) -> (t,ty)

> findCtxt_DB_aux :: DB_Index -> Ctxt -> (Name, Term, Type)
> findCtxt_DB_aux i (Ctxt es)
#ifndef PRODUCTION
>  | offset < 0 = error $ "FCD " ++ show i ++ "\n" ++ unlines (map show es)
>  | null $ drop offset bvs 
>     = trace ("Not enough bound vars (" ++ show i ++ ")\n"
>				++ unlines (map (take 200 . show) es) ++ "END\n") $ 
>		let si = "(" ++ show i ++ ")"
>       in (Plain $ "BAD ID" ++ si, 
>           Var $ Plain $ "BAD LET ID:" ++ si,
>           Var $ Plain $ "Not enough bound vars " ++ si)

<>     = error $ "Not enough bound vars (" ++ show i ++ ")\n" 
<>	               ++ unlines (map (take 200 . show) es) ++ "END\n"

#endif
>  | otherwise = bvs !! offset
>  where
>		bvs = [ (n, shift_over (unpack_let cek), shift_over ty) 
>		       | (n,CE cek ty) <- es, is_DB_binder cek ]
>		offset = i - db_start
>		shift_over = shift_scc (offset + 1)	-- shift over this many entries.
>		shift_scc = _scc_ "ctxt_shift" shiftUnderSomeBinders
>				-- just for profiling purposes, give this use of shift a name.
>		unpack_let (Let_ t) = t
>		unpack_let _        = error $ "Not a Let: " ++ show i ++ "\n" ++ show es

---
`add_ctxt' is internal function.
though it also has uses in checking SimpleTerms since it hides previous names.

> add_ctxt :: Name -> Ctxt_entry -> Ctxt -> Ctxt
> add_ctxt i e (Ctxt es) = Ctxt $ (i,e):es

---
`addCtxt' is the official function. 
* returns new context, else an error. 
* assumes type checking has been done, where necessary.
* underscores don't need to be renamed or anything - just act as placeholders

> addCtxt :: BoundName -> Ctxt_entry -> Ctxt -> OkF Ctxt

> addCtxt Underscore ce c     
>  = Ok $ add_ctxt name ce c
>    where
>		name = Plain "_"
>		-- disallow this? or use new name? 
>		-- but it IS useful when inside terms, and things ARE really __

> addCtxt (Bound n)  ce c
>  | isOk $ findCtxt n c = Fail $ "Name already in ctxt: " ++ show n 
>  | otherwise           = Ok   $ add_ctxt n ce c

---
addCtxt_rn
 - adds Ctxt_entry, _renaming_ bound var if necessary.
 - underscores get mapped to "x-something"

 - renaming is necessary to avoid conflict with already-bound name
 - done lazily, so only costs if the name is used.
 - (case where new namespace is useful???)

> underscore_name_ = Plain "_"

> addCtxt_rn :: BoundName -> Ctxt_entry -> Ctxt -> (Ctxt, Name)
> addCtxt_rn Underscore ce c 
>  = addCtxt_rn (Bound $ Plain "x") ce c
>      -- WAS: no rename, (add_ctxt underscore_name_ ce c, underscore_name_)

> addCtxt_rn (Bound p) ce c 
>  = let m = case freeNamesOf c p of
>		[]  -> error $ "No names left in addCtxt_rn, for: " ++ show p
>		n:_ -> n
>    in (add_ctxt m ce c, m)

> addCtxt_rn other ce c = error $ "addCtxt_rn missing case: " ++ show other

might complain, haven't got round to adding cases for operator names. 
 - might disallow renaming, since we SHOULD NOT use op names ambiguously? 
 - NB careful of contradictions, eg subst under => inner HAS to be renamed.

---
`freeNamesOf'
  - the free name generator for names based on n, free in c.

> type FreeNameSupply = Name -> [Name]

> freeNamesOf :: Ctxt -> FreeNameSupply
> freeNamesOf (Ctxt es)
>  = freeNamesOf_ (\n -> isNothing $ ctxt_lookup n es)

-- \n -> n `elem` map fst es)
-- OR lookup might give better performance. 
-- prefer to try lookup - might be more optimised.

> freeNamesOf_fast :: Ctxt -> FreeNameSupply
> freeNamesOf_fast c
>  = freeNamesOf_ (isNothing . lookupFM table)
>    where
>	table = {-# SCC "table-fast_freeNamesOf" #-} (new_table c)

---

> freeNamesOf_ lu_fn (Operator n) = []		
>  -- can't change the name here.

> freeNamesOf_ lu_fn (Plain n)
>  = [ n' | n' <- n_plus_numbers, lu_fn n' ]
>    where 
>		n_plus_numbers = map Plain $ n : [ mk_id i | i <- [1..] :: [Integer] ]
>		xiferp = dropWhile isDigit $ reverse n
>		mk_id i = foldl (flip (:)) (show i) xiferp

> freeNamesOf_ lu_fn (CoName (CN [n])) 
>  = [ n' | n' <- n_plus_numbers, lu_fn n' ]
>    where 
>		mk_cn = CoName . CN . (:[])
>		n_plus_numbers = map mk_cn $ n : [ mk_id i | i <- [1..] :: [Integer] ]
>		xiferp = dropWhile isDigit $ reverse n
>		mk_id i = foldl (flip (:)) (show i) xiferp

> freeNamesOf_ lu_fn (CoName n) 
>  = error $ "freeNamesOf: on non-simple coercion name " ++ show n

> freeNamesOf_ lu_fn (Backticks n) 
>  = [ Backticks n' | Plain n' <- freeNamesOf_ lu_fn (Plain n) ]
>  -- based on name supply for normal ids. 
>  -- this needs tightening up, to avoid confusion! 




---
MetaVar - name convention.
 - main creation will be via freeNamesOf, on basis of first_numbered_metavar 
 - the prefix def can be hidden.

> metavar_prefix = "m_" :: String
> first_numbered_metavar = Plain $ metavar_prefix ++ show 0 :: Name


---
`addCtxt_BV' and `addCtxt_Let'
  - special cases
  - We don't want the changed name back, since we will only refer to it via
	DB_Index from now on.

BUT! - what about giving terms in proof context? we have to reverse the
process? NO - user knows which terms he means. 

> addCtxt_BV  :: BoundName -> Type -> Ctxt -> Ctxt
> addCtxt_BV n ty = fst . addCtxt_rn n (CE (Bind "addCtxt_BV") ty)

> addCtxt_Let :: Name -> Term -> Type -> Ctxt -> Ctxt
> addCtxt_Let i t ty = fst . addCtxt_rn (Bound i) (CE (Let_ t) ty)


---
`chgCtxt' changes first occ of name by applying function.
	Coded this way to stop after find.
	ASSUMING: that id is present; error condition should not occur.

> chgCtxt :: Name -> (Ctxt_entry -> OkF Ctxt_entry) -> Ctxt -> OkF Ctxt
> chgCtxt i f (Ctxt es)
>  | null post = fail_with_msg $ "not found in chgCtxt: " ++ show i
>  | otherwise = do 
>					ce' <- embed $ f ce 
>					return $ Ctxt $ pre ++ (i, ce') : rest
>  where
>		(pre,post@((_,ce):rest)) = break (\e -> fst e == i) es

  

%-------------------------------------------------------------------------------
`split_ctxt'
 - does lookup for given id, and returns the context added after it with
   the context before it.
 - Fails if not found.

> split_ctxt :: Name -> Ctxt -> OkF ([ID_CE], Ctxt_entry, [ID_CE])
> split_ctxt i (Ctxt es)
>  = case span (\e -> fst e /= i) es of
>		(post, [])          -> Fail $ "split_ctxt: " ++ show i ++ " not found."
>		(post, (_, ce):pre) -> Ok (post, ce, pre)


---
`findNameInCtxt'
  - returns context_entry with the context in which name is defined. 
  - This is used in the implementation of CUT.
  - Also returns a context containing just the definitions after i.

PLAN: to change the postdefs in to Lets.

> findNameInCtxt :: Name -> Ctxt -> OkF (Ctxt, Ctxt_entry, PostDefs)
> findNameInCtxt i c
>  = change_msg (\m -> "'" ++ show i ++ "' not found in context") $ 
>    do 
>		(post, ce, tail) <- split_ctxt i c
>		return (Ctxt tail, ce, PostDefs $ Ctxt $ filter (is_def.snd) post)


---
`PostDefs' and `PreDefs'
  - small abstraction to hide details of expanding possibly non-WF definitions
	in a term.
  - we can expand terms in it,
  - or add it to a ctxt for purposes of (eg) translate_db.

NB might have to include defs with SubCtxt if going to preserve
definitions...

> newtype PostDefs = PostDefs SubCtxt
> newtype PreDefs  = PreDefs  SubCtxt

---
`expandSomeDefs'
 - expand the defs in the subctxt throughout the term.
 - nb have to recursively expand definition rhs's too.

> expandSomeDefs :: PostDefs -> Term -> Term
> expandSomeDefs (PostDefs (Ctxt ds))
>  = replaceTerminal (replace_def $ map fst ds)

> replace_def ds (GlobalDef i rhs)
>  | i `elem` ds = Just $ replaceTerminal (replace_def ds) rhs
> replace_def ds t = Nothing


---
`processWithDefs'
  - temporarily extend context with post-defs
  - DOESN'T ensure that defs are WF in the base Ctxt 
  - ASSUME that later checking (eg type checking) will catch anomalies.
  - The continutation style is some protection against defs escaping - 
  - BUT 'a' could be Ctxt etc etc. 

> processWithDefs :: PostDefs -> (Ctxt -> a) -> Ctxt -> a
> processWithDefs (PostDefs (Ctxt ds)) f (Ctxt es)
>  = f (Ctxt $ ds ++ es)


%-------------------------------------------------------------------------------
Tables
 - want chain to previous one

> table_name :: Name
> table_name = Plain "_table_"

> fix_table :: Ctxt -> Ctxt
> fix_table c@(Ctxt es)
>  = Ctxt $ (table_name, CE (Table $ new_table c) ce_dummy_type_) : es

> new_table :: Ctxt -> FiniteMap Name Ctxt_entry
> new_table c@(Ctxt es)
>  = case split_ctxt table_name c of
>	Fail _                       -> listToFM es
>	Ok (post, CE (Table t) _, _) -> t `extend` post
>    where
>	drop_old = \old new -> new
>	t `extend` id_ces 
>	 = foldr (\(key,elt) t -> addToFM_C drop_old t key elt) t id_ces
>	  -- using foldr so that correct context visibility is maintained.
>	  -- WAS: t `extend` id_ces = addListToFM t (reverse id_ces)






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
`Ctxt_entry' - all entries have specific info plus a Type.

> data Ctxt_entry 
>  = CE !Ctxt_entry_kind !Type 
>    deriving Show


%---------
`Ctxt_entry' represents the kind of entry plus supplemental info.

the atom field is store for the term to be produced from this context-entry

> data Ctxt_entry_kind
>  = Def 				-- global def
>		{rhs :: Term,
>		 atom :: Term}

>  | Hyp  				-- (global) hyp
>		{atom :: Term}

>  | M_V				-- metavar.
>		{atom :: Term}

-- QQ: should discharge only work on the global context? 

>  | Let_				-- local def
>		{val :: Term}

>  | Bind 				-- bound var assumed during type analysis
>		{binder :: String}	-- WANT INFO ON KIND OF BINDING 

QQ: need identify the kind of binding? 
QQ: do all bindings behave as I expect lambda to? (what about sigma?)
(this is why no ident is used at present).

%---------

>  | ElimOp					-- identifies an Elimination Operator.
>		{e_arity :: Arity
>		,atom    :: Term}

>  | IndFamily				-- (name of) Inductive type
>		{constrs :: [Name]	-- names of the constructors of the type.
>		,atom    :: Term
>		,input   :: String}	-- the original spec, for dumping later
>					-- use this CEK to "register" proper ind types.

>  | FnDef 					-- for arbitrary rewrite rules (including auto-
>		{arity :: Arity		-- generated induction rules)
>		,rhs   :: Term
>		,atom  :: Term}	

%---------

>  | Coercion
>		{term    :: Term	-- the coercion term
>       ,mv_term :: Term	-- saturated with ?'s to provide final kind (A->B)
>		,dom     :: Type	-- 
>		,rng     :: Type	-- forms for matching. 
>		,co_rule :: Bool	-- is it a coercion rule (eg map over lists)?
>		,atom    :: Term	-- the coercion term, expressed as definition
>		}

%---------

>  | IntrosContinuation		-- term produced by intros
>		{intros_id :: Name,	-- id of term being built in sub-ctxt.
>		 ctu :: Ctxt_Ctu}	-- context to continue with, after replacing term.

%---------
Dynamically generated context entries.
 - the technique is to create the necessary term when a "magic name" appears
   in the concrete syntax. 
 - CAST is often used, to indicate the type of the dynamically created term.
 - much used for universes, to allow potentially infinite set of identifiers.

>  | Dynamic (Ident -> Int -> OkF Term)	
>				-- give it name prefix and numeric suffix.
>				-- it can fail, eg restriction on univ levels.

%---------
Faster lookup tables
 - strict table

>  | Table !(FiniteMap Name Ctxt_entry)

%---------
Misc Stuff

>  | Mark		-- for context subtraction
>  | ModuleMark		-- for detecting what is loaded, name is module.

>  | FileOpen 
>  | FileCalled 
>  | FileClose		-- maybe include file dependencies? 

>  | UserSyntax UserSyntaxInfo
>  deriving Show

> type Ctxt_Ctu = Term -> OkF Ctxt

> instance Show (FiniteMap a b) where
>	show _ = "<TABLE>"


%---------------------------------------


> isHyp :: Ctxt_entry_kind -> Bool
> isHyp (Hyp _) = True
> isHyp _       = False

---
is_def' - just for local purposes.

> is_def :: Ctxt_entry -> Bool
> is_def (CE (Def _ _) _) = True
> is_def ce               = False

---
`is_DB_binder'
  - is a binding form which is indexed by DB system

> is_DB_binder (Bind _) = True
> is_DB_binder (Let_ _) = True
> is_DB_binder _        = False

---
Test for hyp-like thing

> is_hyp_or_metavar (Hyp _) = True
> is_hyp_or_metavar (M_V _) = True
> is_hyp_or_metavar _       = False

---
`type_of'
  - get type from a CE

> type_of :: Ctxt_entry -> Type
> type_of (CE _ ty) = ty


---
`ce_dummy_type'
  - used to fill type slot in CE for things like file info.
  - takes arg, to indicate source of the dummy.

> ce_dummy_type_ :: Type
> ce_dummy_type_ = ContextVar (Plain "CE-Dummy-Type")

> ce_dummy_type :: String -> Type
> ce_dummy_type s = ContextVar $ Plain $ "CE-Dummy-Type-" ++ s


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Principle of ordering of 2 vars.

Tests:
 - if both vars are in C. 
 - gets pre-contexts of both.
 - then the comparison reduces to comparison of sizes of pre-ctxts.

NB relatively expensive... - but probably not time-critical.
   if it becomes time-critical, can be slopping about context checks
   (eg, v1==v2 -> Eq, and first_found is smallest without finding second)
   (ps - this means we don't need the OkF wrapping?)

> orderVars :: Ctxt -> Name -> Name -> OkF Ordering
> orderVars c n1 n2
>  = change_msg (("orderVars " ++ show (n1,n2) ++ ":") ++) $
>    find_first n1 n2 c

--- 
`find_first'
  - pass through context, looking for first of the two names.
  - NB both names must be found in the context.
  - so if one found, we check to find the other in the remaining context.
  - otherwise, record a fail.

> find_first :: Name -> Name -> Ctxt -> OkF Ordering 
> find_first n1 n2 (Ctxt [])
>  = Fail $ "Neither name found"
> find_first n1 n2 c@(Ctxt ((n, _):es))
>  | n1 == n 
>     = if n2 == n 
>       then Ok EQ 
>       else findCtxt n2 c >> Ok GT
>  | n2 == n
>     = findCtxt n1 c >> Ok LT
>  | otherwise
>     = find_first n1 n2 (Ctxt es)


%-------------------------------------------------------------------------------
`localiseDefs'
  - convert defs in sub-context to local bindings in a given term.
  - NB context order means latest first, hence the use of left fold.

> localiseDefs :: SubCtxt -> Term -> Term
> localiseDefs (Ctxt ds) rhs
>  = snd $ foldl localise_one_def (defs_in rhs, rhs) ds

---
Defs OR M_Vs - by consequence of offloading substitutions.

> defs_in t = catMaybes $ map nameFromTerm $ subtrees def_or_mv t
> def_or_mv (GlobalDef _ _) = True
> def_or_mv (MetaVar _)     = True
> def_or_mv _               = False


> localise_one_def dst@(ds, t) (n, CE (Def d _) _)
>  | n `elem` ds = (defs_in d ++ ds, makeContextVar_Binding (Let. unbind) n d t)
>  | otherwise   = dst

> localise_one_def dst (n, CE cek _) = dst



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
MONAD STUFF FROM HERE ON.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Class of overloaded operations for state monads which contain context

> class Monad m => ContextM m where
>	find   :: Name -> m Ctxt_entry
>	c_run  :: Ctxt -> m a -> OkF a
>	change :: (Ctxt -> Ctxt) -> m a -> m a
>	ctxt   :: m Ctxt
>	add    :: Name -> Ctxt_entry -> m a -> m Name

<>	add i e = change (addCtxt i e) 

CHANGE maybe a problem since distribution via bind is separate mechanism?
  ie would like to have it as monad former, rather than monad transformer.

Monad context for class MIGHT NOT BE NECESSARY.
  BUT it will usually be satisfiable.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
State Transforming Monads which use Context

`CM' is an instantiation of M_, that has Ctxt for state.

> type CM a = M_ Ctxt a

---
`IM' is a fixed instantiation of (CM a), and is used for the top level of the
program. 

> type IM   = CM ()


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Error-handling read monad with Ctxt as State.

> newtype CERM a = CERM (Ctxt -> OkF a)
> extract_CERM (CERM m) = m
> through_CERM f (CERM m) = CERM $ f m

> instance Functor CERM where
>	fmap f (CERM m) = CERM $ \c -> fmap f (m c)

> instance Monad CERM where
>   return a     = CERM $ \c -> return a
>   CERM m >>= k = CERM $ \s -> m s >>= \a -> let (CERM k') = k a in k' s
#ifdef __HASKELL98__
>   fail s = CERM $ \c -> fail s
#else 
> instance MonadZero CERM where
>	zero = CERM zero		-- nb we lose specificity of error mesg
#endif

> instance Fallible CERM where
>	fail_with_msg s = CERM $ \c -> fail_with_msg s
>	change_msg mf = through_CERM (\m c -> change_msg mf $ m c)
>	try_another (CERM m) f
>	 = CERM 
>	 $ \c -> case m c of Fail s -> extract_CERM (f s) c
>		             ok     -> ok 

----

> instance ContextM CERM where
>	c_run c m = extract_CERM m c
>	ctxt      = CERM return 
>	change f  = \(CERM m) -> CERM (\c -> m (f c))
>	find i    = CERM (findCtxt i)



> useStateCERM :: (Ctxt -> OkF a) -> CERM a
> useStateCERM f = CERM f


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
