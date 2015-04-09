> module TermReduction (

Big operations on terms

#define WANT_ALL_EXPORTED
#undef WANT_ALL_EXPORTED
#ifdef WANT_ALL_EXPORTED
>			-- module TermOps
#else
>			substitute, 
>			substitute_star, 

>			shiftUnderOneBinder,
>			shiftUnderSomeBinders,
>			shiftOverOneBinder,
>			shiftOverSomeBinders,

>			reduceAppliedAbstrs,

>			-- for inlining ONLY! 
>			-- sdb, 
>			-- shift, s_,				- doesn't affect much.
>			-- for inlining ONLY! 

>			compileReductionAtom,
>			compileArbitraryTerm,
>			compileArbitraryDefTerm,

>			flatten, flatten_aux, flattenAp

>			, first_IVar
>			, ivar_caf
>			, test_ivars
>			, db_start			-- should not export really. need abstr.
>			, BindingDepth(..)
#endif

>		) where


#if __GLASGOW_HASKELL__ < 302
> import Trace
#else
> import IOExts_(trace)
#endif
> import Data.Maybe(catMaybes)
> import Data.List(partition, intersect, (\\))
> import Control.Monad

> import DebugOptions(traceOnDebugFlag)
> import Base
> import Terms
> import SharedSyntax

> import Terms(isAtom)		-- to keep name in .hi file...

<> import Debugging(hack_show)
Not good, because of mut-rec. 

> hack_show :: Term -> String
> -- hack_show = take 500 . pretty_crude_print
> hack_show = take 500 . crude_print

%-------------------------------------------------------------------------------
NOTES:

compare seems to be better than tests on integer subtraction.
(at least, explicit ifs and signum can be slower, if not careful - bcos Bool?)
maybe change if unboxed types used? 

idea 
 - make applications of ivar_caf strict when adding etc
 - might need optimised vsn to see difference? 



%-------------------------------------------------------------------------------

> r_tracef f r = r_trace (f r) r 
> r_trace s = traceOnDebugFlag "REDUCTION_TRACING" (s `fseq` s)

> c_trace s = traceOnDebugFlag "COMPILATION_TRACING" (s `fseq` s)

<> r_traceON s = r_trace s
<> -- r_traceON s = traceOnDebugFlag "" (s `fseq` s)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Cmt? 

---
Start de Bruijn indexing from here.

> type BindingDepth = Int
> db_start = 0 :: BindingDepth

%-------------------------------------------------------------------------------
Reduce the number of allocations of these terms.
Cater for the first 20. 

> ivar_caf :: DB_Index -> Term
> ivar_caf  0 = ivar_0
> ivar_caf  1 = ivar_1
> ivar_caf  2 = ivar_2
> ivar_caf  3 = ivar_3
> ivar_caf  4 = ivar_4
> ivar_caf  5 = ivar_5
> ivar_caf  6 = ivar_6
> ivar_caf  7 = ivar_7
> ivar_caf  8 = ivar_8
> ivar_caf  9 = ivar_9
> ivar_caf 10 = ivar_10
> ivar_caf 11 = ivar_11
> ivar_caf 12 = ivar_12
> ivar_caf 13 = ivar_13
> ivar_caf 14 = ivar_14
> ivar_caf 15 = ivar_15
> ivar_caf 16 = ivar_16
> ivar_caf 17 = ivar_17
> ivar_caf 18 = ivar_18
> ivar_caf 19 = ivar_19
> ivar_caf  n = IVar n

cafs for the ivars

> ivar_0  = IVar 0  :: Term
> ivar_1  = IVar 1  :: Term
> ivar_2  = IVar 2  :: Term
> ivar_3  = IVar 3  :: Term
> ivar_4  = IVar 4  :: Term
> ivar_5  = IVar 5  :: Term
> ivar_6  = IVar 6  :: Term
> ivar_7  = IVar 7  :: Term
> ivar_8  = IVar 8  :: Term
> ivar_9  = IVar 9  :: Term
> ivar_10 = IVar 10 :: Term
> ivar_11 = IVar 11 :: Term
> ivar_12 = IVar 12 :: Term
> ivar_13 = IVar 13 :: Term
> ivar_14 = IVar 14 :: Term
> ivar_15 = IVar 15 :: Term
> ivar_16 = IVar 16 :: Term
> ivar_17 = IVar 17 :: Term
> ivar_18 = IVar 18 :: Term
> ivar_19 = IVar 19 :: Term

---

> first_IVar :: Term
> first_IVar = ivar_0


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Substitution in DeBruijn Terms.
 - replace occurrences of the outermost bound var in the first term with 
   the second term.
 - 

<> {-# INLINE substitute #-}

> substitute :: Term -> Term -> Term
> substitute body arg = sdb arg db_start body

--- 

<> {-# INLINE sdb #-}

> sdb :: Term -> BindingDepth -> Term -> Term

The important cases

> sdb a d t@(IVar i) 
>  = case (compare i d) of
>		LT -> t					-- bound var.
>		GT -> ivar_caf (i-1)	-- free vars - one less binder above it
>		EQ -> shift d a 		-- substitute here, shift term to depth d

> sdb a d t@(LocalDef i rhs)
>  = case (compare i d) of
>		LT -> t					-- bound var.
>		GT -> LocalDef (i-1) (sdb a d rhs)
>								-- free vars - one less binder above it
>		EQ -> shift d a			-- substitute here, shift term to depth d


The simple cases - just return self.

> sdb a d t@(Type)          = t
> sdb a d t@(Kind)          = found_Kind "substitute" 

> sdb a d t@(Elim _ _)      = t
> sdb a d t@(F_Elim _)      = t
> sdb a d t@(Const _ _)     = t
> sdb a d t@(F_Const _ _)   = t
> sdb a d t@(Closed _ _)    = t  -- term is CLOSED, so no subst necessary! 

> sdb a d t@(Var _)         = t		-- should these remain? 
> sdb a d t@(GlobalDef _ _) = t
> sdb a d t@(ContextVar _)  = t

> sdb a d t@(MetaVar _)     = t		-- NOT COMPLETELY SURE 
>									-- might need some special treatment? 


The recursive cases - just propagate function.

> sdb a d (El t)
>  = El $ sdb a d t

> sdb a d (DP v ty t)
>  = DP v (sdb a d ty) (sdb a (d+1) $ t)

> sdb a d (FO v ty t)
>  = FO v (sdb a d ty) (sdb a (d+1) $ t)

> sdb a d (Let v t1 t2)
>  = Let v (sdb a d t1) (sdb a (d+1) $ t2)

> sdb a d (Ap t1 t2)
>  = Ap (sdb a d t1) (sdb a d t2)

> sdb a d (Co fn arg)
>  = Co (sdb a d fn) (sdb a d arg)

> sdb a d (Ext (SoftCast e t))
>  = Ext $ SoftCast (sdb a d e) (sdb a d t)

> sdb a d (Ext (HardCast e t))
>  = Ext $ HardCast e (sdb a d t)			-- don't visit e.

> sdb a d (Ext e)
>  = Ext $ fmap (sdb a d) e



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Optimised version - replaces several terms at same time. 


> vs = map (Var. Plain.(:[])) :: String -> [Term]
> ab = vs "AB"
> f0 = (FO Underscore Kind (FO Underscore Kind (Ap (Ap (IVar 1) (IVar 0)) (FO Underscore Type (Ap (IVar 2) (Ap (IVar 0) (IVar 3)))))))


> ss as (FO _ _ t) = ss as t
> ss as t          = substitute_star (reverse as) t

---
`substitute_star'
  - IMPORTANT - expects arguments to be in reverse order, as done by f_/f_star

---
`substitute_star'
  - faster version of substitute_star, doesn't carry around the binding 
	depth; when it hits a binder, jumps to substitute_star

  - Can use direct (!!) since there are no FO binders to shift over (but
	see note above). 

<> {-# INLINE substitute_star #-}

> substitute_star :: [Term] -> Term -> Term
> substitute_star args
#if 1
>  = sdb_star_top args			-- to experiment with
#else
>  = sdb_star_entry args		-- when optimisation used...
#endif

---
`sdb_star_top'
  - simplified version for terms without binders and IVars
  - means extra parameters (shift count and depth) can be omitted
  - calls normal sdb_star when we find a binder
  - NB doesn't pre-calc the length, which might be expensive if reducing
		UNDER binders; might have to examine big examples to see which better


<> {-# INLINE sdb_star_top #-}

> sdb_star_top :: [Term] -> Term -> Term

> sdb_star_top as (Ap l r)     
>  = Ap (sdb_star_top as l) (sdb_star_top as r)
> sdb_star_top as (Co l r)     
>  = Co (sdb_star_top as l) (sdb_star_top as r)
> sdb_star_top as (El t)
>  = El (sdb_star_top as t)

> sdb_star_top as t@(Type)          = t
> sdb_star_top as t@(Elim _ _)      = t
> sdb_star_top as t@(F_Elim _)      = t
> sdb_star_top as t@(Const _ _)     = t
> sdb_star_top as t@(F_Const _ _)   = t
> sdb_star_top as t@(ContextVar _)  = t
> sdb_star_top as t@(GlobalDef _ _) = t
> sdb_star_top as t@(MetaVar _)     = t

> sdb_star_top as t@(Closed _ _)    = t
>    -- NB point of Closed is that we DO NOT need to subst in outer vars, 
>    --    (because term is closed) - so we don't need to change _anything_


Anything else, just call normal sdb_star

> sdb_star_top as t = sdb_star_entry as t


---
Safety check.
 - is usually off, unless I am changing the reduction mechanism

<> {-# INLINE guarded_lu #-}

#define NO_LOOKUP_CHECKS

#ifdef NO_LOOKUP_CHECKS

> guarded_lu :: String -> [Term] -> Int -> Term
> guarded_lu s as i = as !! i

#else

> guarded_lu :: String -> [Term] -> Int -> Term
> guarded_lu s as_0 i_0
>  = lu as_0 i_0
>    where
>		lu (a:as) 0 = a
>		lu (a:as) (i+1) = lu as i
>		lu as i = error $ "BAD INDEX (guarded_lu):" ++ s ++ " "
>							++ show i_0 ++ "\n" ++ unlines (map show as_0)

#endif


---
`sdb_star_entry' does the necessary init stuff for sdb_star
  - reminder: caller could pre-calc the length, 
  - but extra arg might cost more - need details!
  - NB recalc only needed when we hit a var bound by outer binder.

<> {-# INLINE sdb_star_entry #-}

> sdb_star_entry :: [Term] -> Term -> Term
> sdb_star_entry as t                 
>  = _scc_ "sdb_star_entry"
>    sdb_star num_args as db_start t
>    where
>		num_args = length as		-- was seq'd

---
`sdb_star'
  - does subst on several args at once
  - it could memoise the each n-shifted arg, but for now, tries without.
  - Hypothesis: variables aren't repeated much in a term
	 - although could be wrong when it comes to types! 
	 - need to study this!

<> {-# INLINE sdb_star #-}

> sdb_star :: Int -> [Term] -> BindingDepth -> Term -> Term

The important cases

#define IS_NEG(n) (signum (n) < 0)

#if 1

able to avoid double subtraction by signum? 

> sdb_star j as d t@(IVar i) 
>  = _scc_ "sdb_star:IVar"
>    case (i - d) of
>      i' -> case signum i' of
>               (-1) -> t                  -- bound var, don't touch.
>               _    -> case (compare i' j) of
>                         LT -> shift d (guarded_lu ("S_* " ++ show i) as i')
>                         _  -> ivar_caf (i-j)		-- lift subtr? 

<>  = case (compare i d) of
<>      LT -> t                                 -- bound var, don't touch.
<>      _  -> case (i-d) of
<>              i' -> case (compare i' j) of
<>                      LT -> shift d (guarded_lu ("S_* " ++ show i) as i')
<>                      _  -> ivar_caf (i-j)		-- lift subtr? 

#else

-- both IS_NEG and lets, no #def IS_NEG bad (0.5 slower)
-- both IS_NEG and lets bad (0.3 slower)
-- both IS_NEG bad (0.2 slower)
-- single let -> case, still slower
-- try when optimisation is used.
-- but looks like compare is the best way to work...

> sdb_star j as d t@(IVar i) 
>  = case (i-d) of { i_minus_d -> 
>      if IS_NEG(i_minus_d)
>      then t 					-- bound var, don't touch
>      else 	 				-- free, shift or substitute
>           if IS_NEG(i_minus_d - j) 
>           then shift d (guarded_lu ("S_* " ++ show i) as i_minus_d)
>           else ivar_caf (i-j)
>      }
#endif

> sdb_star j as d t@(LocalDef i rhs)
>  = case (compare i d) of
>		LT -> t		
>		_  -> case (i-d) of
>				i' -> case (compare i' j) of
>						LT -> error "S_* on Lets, shouldn't happen" 
>						_  -> LocalDef (i-j) (sdb_star j as d rhs)


The simple cases - just return self.

> sdb_star j as d t@(Type)          = t
> sdb_star j as d t@(Kind)          = found_Kind "substitute" 

> sdb_star j as d t@(Elim _ _)      = t
> sdb_star j as d t@(F_Elim _)      = t
> sdb_star j as d t@(Const _ _)     = t
> sdb_star j as d t@(F_Const _ _)   = t

> sdb_star j as d t@(Closed _ _)    = t
>    -- NB point of Closed is that we DO NOT need to subst in outer vars, 
>    --    (because term is closed) - so we don't need to change _anything_

> sdb_star j as d t@(Var _)         = t		-- should these remain? 
> sdb_star j as d t@(GlobalDef _ _) = t
> sdb_star j as d t@(ContextVar _)  = t
> sdb_star j as d t@(MetaVar _)     = t


The recursive cases - just propagate function.

> sdb_star j as d (El t)
>  = El $ sdb_star j as d t

> sdb_star j as d (DP v ty t)
>  = DP v (sdb_star j as d ty) (sdb_star j as (d+1) t)

> sdb_star j as d (FO v ty t)
>  = FO v (_scc_ "sdb_star:FO_Ty" sdb_star j as d ty) 
>         (_scc_ "sdb_star:FO_Ex" sdb_star j as (d+1) t)

> sdb_star j as d (Let v t1 t2)
>  = Let v (sdb_star j as d t1) (sdb_star j as (d+1) t2)

> sdb_star j as d (Ap t1 t2)
>  = _scc_ "sdb_star:Ap"
>    Ap (sdb_star j as d t1) (sdb_star j as d t2)

> sdb_star j as d (Ext (SoftCast e t))
>  = Ext $ SoftCast (sdb_star j as d e) (sdb_star j as d t)

> sdb_star j as d (Ext (HardCast e t))
>  = Ext $ HardCast e (sdb_star j as d t)			-- don't visit e.

> sdb_star j as d (Co fn arg)
>  = Co (sdb_star j as d fn) (sdb_star j as d arg)




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Adjustment of DeBruin Indices 

---
Abstraction for common shift uses.
 - helps abstraction and protection.

`shiftUnderOneBinder'   - when adding a new outer binder.
`shiftUnderSomeBinders' - when moving a term under several new binders
`shiftOverOneBinder'    - when moving a term from underneath a binder.

<> {-# INLINE shiftUnderOneBinder   #-}
<> {-# INLINE shiftUnderSomeBinders #-}
<> {-# INLINE shiftOverOneBinder    #-}

> shiftUnderOneBinder   = shift 1		::        Term -> Term
> shiftOverOneBinder    = shift (-1)	::        Term -> Term

> shiftOverSomeBinders n = shift (-n)	::        Term -> Term

> shiftUnderSomeBinders :: Int -> Term -> Term
> shiftUnderSomeBinders n
>  | is_negative n = \t -> error $ msg ++ crude_print t
>  | otherwise     = shift n
>  where
>		msg = "shiftUnderSomeBinders NEGATIVE (" ++ show n ++ "):\n"

---

<> {-# INLINE shift #-}
<> {-# INLINE s_    #-}

> shift :: Int -> Term -> Term
> shift 0 t 			-- probably easier to catch here than to prevent? 
>  = t					-- unbox here too
> shift n t 
>  = -- _scc_ "shift_def" 
>    s_ n db_start t

---
CODED AS EXPLICIT STATE-PASSING RECURSION for speed.

> s_ :: Int -> BindingDepth -> Term -> Term		

The important cases

#if 1

-- I don't think this makes much difference.

> s_ j d t@(IVar i) 
>  = _scc_ "s_:IVar"
>    case (compare i d) of
>		LT -> t						-- t is bound variable, no change
>		_  -> ivar_caf (i + j)	 	-- shift by j.

#else

> s_ j d t@(IVar i) 
>  = if IS_NEG(i-d) 
>    then t						-- t is bound variable, no change
>    else ivar_caf (i + j)	 	-- shift by j.

#endif

> s_ j d t@(LocalDef i rhs)
>  = case (compare i d) of
>		LT -> t						-- t is bound variable, no change
>		_  -> LocalDef (i + j) (s_ j d rhs)
>									-- shift by j.


The simple cases - just return self.

> s_ j d t@(Kind)          = t -- DISABLE found_Kind "shift"
> s_ j d t@(Type)          = t

> s_ j d t@(Const _ _)     = t
> s_ j d t@(F_Const _ _)   = t
> s_ j d t@(Closed _ _)    = t  -- no change, since term is closed.
> s_ j d t@(Elim _ _)      = t
> s_ j d t@(F_Elim _)      = t

> s_ j d t@(GlobalDef _ _) = t
> s_ j d t@(ContextVar _)  = t
> s_ j d t@(MetaVar _)     = t

> s_ j d t@(Var i)         = t

WAS:
<> s_ (Var i) = error $ "INVALID_VAR_IN_SHIFT: " ++ show i
BUT:
it is a harmless operation - not ideal, since it indicates comp on a raw term
however, I'm not so fussy in sdb! (so: inconsistent!)



The recursive cases - just propagate function, adjusting binding depth 
	where appropriate.

> s_ j d (El t)
>  = El (s_ j d t)

> s_ j d (DP v ty t)
>  = DP v (s_ j d ty) (s_ j (d+1) $ t)

> s_ j d (FO v ty t)
>  = FO v (s_ j d ty) (s_ j (d+1) $ t)

> s_ j d (Let v t1 t2)
>  = Let v (s_ j d t1) (s_ j (d+1) $ t2)

> s_ j d (Ap t1 t2)
>  = Ap (s_ j d t1) (s_ j d t2)

> s_ j d (Co f a)
>  = Co (s_ j d f) (s_ j d a)

> s_ j d (Ext (SoftCast e t))
>  = Ext $ SoftCast (s_ j d e) (s_ j d t)

> s_ j d (Ext (HardCast e t))
>  = Ext $ HardCast e (s_ j d t)			-- don't visit e.

> s_ j d (Ext e) 
>  = Ext $ fmap (s_ j d) e



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
COMMENTS!

not closed term with arity n, ie term uses IVars above the given range.

> not_closed_term :: Arity -> Term -> Bool
> not_closed_term a = nct db_start

> nct d (Kind)           = found_Kind "nct"
> nct d (Type)           = False
> nct d (Elim _ _)       = False
> nct d (F_Elim _)       = False
> nct d (Const _ _)      = False
> nct d (F_Const _ _)    = False
> nct d (Closed _ _)     = False		-- because it is a closed term.
> nct d (Var _)          = False		-- needed for printing terms.
> nct d (GlobalDef _ _)  = False		-- GlobalDef won't contain external ix's
> nct d (ContextVar _)   = False
> nct d (MetaVar _)      = False

> nct d (IVar i)         = i >= d

> -- WAS nct d (LocalDef i _)   = i >= d && p (i - d)
> -- but: local def could contain bvs from higher?  
> -- [nov00 - check this]
> nct d (LocalDef i def) = i >= d

> nct d (El t)           = nct d t
> nct d (Ap f a)         = nct d f || nct d a
> nct d (Co f a)         = nct d f || nct d a

> nct d (DP v ty t)      = error "nct on DP" -- shouldn't occur.
> nct d (FO v ty t)      = nct (d+1) t
> nct d (Let v t1 t2)    = nct d t1 || nct (d+1) t2

> nct d (Ext (SoftCast e t)) = nct d e -- || nct d t
> nct d (Ext (HardCast e t)) = nct d e -- || nct d t		-- DEBATE!
> -- NB type expressions here should be ignored.

> nct d (Ext e) = fold_Extension or (nct d) e		-- General, check it


%-------------------------------------------------------------------------------

NEED STRICTNESS HERE, in i EH? - PROBABLY NOT!!! (mar99)

Note: used outside ONLY to test usage of an IVar. - and then only applied
	  for first IVar (eg printing/reduction)
	  SO - possible to rewrite to specific test for this.  [6aug01]

> test_ivars :: (DB_Index->Bool) -> Term -> Bool
> test_ivars p = tv p 0

> tv p d (Kind)           = found_Kind "IVar testing fn"
> tv p d (Type)           = False

> tv p d (Elim _ _)       = False
> tv p d (F_Elim _)       = False
> tv p d (Const _ _)      = False
> tv p d (F_Const _ _)    = False
> tv p d (Closed _ _)     = False		-- because it is a closed term.

> tv p d (Var _)          = False		-- needed for printing terms.
> tv p d (GlobalDef _ _)  = False		-- GlobalDef won't contain external ix's 
> tv p d (ContextVar _)   = False
> tv p d (MetaVar _)      = False

> tv p d (IVar i)         = i >= d && p (i - d)

> -- WAS tv p d (LocalDef i _)   = i >= d && p (i - d)
> -- but: local def could contain bvs from higher?  
> -- [nov00 - check this]
> tv p d (LocalDef i def) = i >= d && p (i - d)

> tv p d (El t)           = tv p d t
> tv p d (Ap t1 t2)       = tv p d t1 || tv p d t2
> tv p d (Co f a)         = tv p d f || tv p d a

> tv p d (DP v ty t)      = tv p d ty || tv p (d+1) t
> tv p d (FO v ty t)      = tv p d ty || tv p (d+1) t
> tv p d (Let v t1 t2)    = tv p d t1 || tv p (d+1) t2

> tv p d (Ext (SoftCast e t)) = tv p d e || tv p d t
> tv p d (Ext (HardCast e t)) = tv p d e || tv p d t		-- DEBATE!

> tv p d (Ext e) = fold_Extension or (tv p d) e		-- General, check it



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
`flatten'
  - reduce to a flat term, just by doing beta reduction on abstractions.
  - first stage of reduction.

> flatten :: Term -> FlatTerm
> flatten t 
>  = r_tracef (\r -> "BF:\n" ++ hack_show t ++ "\n"
>              -- ++ "AF:\n" ++ hack_show (unflatten r) ++ "\n"
>             ) $ 
>    f_ [] t

> flatten_aux = f_

> f_ s t@(Type)          = E t s	-- NB these three should have (s == [])
> f_ s t@(Kind)          = E t s	-- but I let TYPE INFERENCE enforce this
> f_ s t@(El _)          = E t s	-- in order to keep details in one place!

> f_ s t@(Elim _ _)      = E t s
> f_ s t@(F_Elim _)      = E t s

> f_ s t@(Var v)         = E t s
> f_ s t@(IVar i)        = E t s
> f_ s t@(LocalDef _ _)  = E t s
> f_ s t@(GlobalDef i _) = E t s
> f_ s t@(ContextVar v)  = E t s
> f_ s t@(MetaVar v)     = E t s

> f_ s t@(Ap t1 t2)      = f_ (t2:s) t1
> f_ s t@(Co t1 t2)      = f_ (t2:s) t1		-- treat like applications.

---
DEBATE
 -- expand, since it is only a local defn, and for convenience

> f_ s t@(Let v t1 t2)   
#if 0
>  = E t s
#else
>  = _scc_ "LetSubst" 
>    f_ s (substitute t2 t1)
#endif

---
Casts
 - typeless reduction means no more type-checking
 - so can discard? - but XXXXX

<> f_ s t@(Ext (HardCast (Ext (HardCast e t1) t2)
<>  | t1 `conv` t2 .... 		=> need for conversion. NO!

> f_ s t@(Ext (HardCast t1 t2))    = E t s

> f_ s t@(Ext (SoftCast t1 t2))    = E t s


---
Manifest Records.
 - stop here.

> f_ s t@(Ext Sig{})               = E t s
> f_ s t@(Ext Rec{})               = E t s

> f_ s t@(Ext Project{})           = E t s
> f_ s t@(Ext Restrict{})          = E t s



%-------------------

> f_ s t@(DP v ty b)
>  = case s of 
>		[] -> A t 
>		as -> error $ "DP Applied to arguments, not valid.\n" 
>							++ unlines (map pretty_crude_print $ t:as)

WAS <> f_ as dp@(DP v ty t)   = E dp as

---
could see if 1 or 2 args ready before launching FO body test? 

f_star 
  - collect extra reduction arguments and then call substitute_star,
  - result of this is then directly flattened.

> f_ s t@(FO v ty body) 
>  = case s of
>		[]   -> A t
#if 0 
>		a:as -> f_ as (substitute body a)		-- revert to plain, for testing
#else
>		a:as -> case body of 
>			  FO _ _ body2 
>				 -> case as of
>						b:bs -> f_star bs [b,a] body2
>						[]   -> {-#_scc_ "partial f_"#-}
>					                case v of
>								  Underscore -> f_ [] $ shiftOverOneBinder body 
>								  Bound v    -> {-#_scc_ "partial bound" #-}f_ [] $ substitute body a
>			  t2 -> {-# _scc_"f_ sgl FO" #-}
>			        case v of
>					  Underscore -> f_ as $ shiftOverOneBinder body
>					  Bound v    -> f_ as $ substitute body a
>    where
>		f_star :: [Term] -> [Term] -> Term -> FlatTerm
>		f_star (a:as) ps (FO _ _ t) = f_star as (a:ps) t
>		f_star as     ps t          = fstar_trace ps t $ 
>		                              f_ as (substitute_star ps t)
>		fstar_trace ps t r 
>		 = r_trace ("BF_*:\n" ++ hack_show t ++ "\n" 
>		                        ++ unlines (map hack_show ps) ++ "\n"
>		      -- ++ "AF_*:\n" ++ hack_show (unflatten r) ++ "\n"

>		           ) r
#endif

%-------------------

> f_ s t@(Const _ _)     = E t s
> f_ s t@(F_Const _ _)   = E t s

> f_ s (Closed _ fn) 
>  = _scc_ "f_:closed"
>    fn s



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
`reduceAppliedAbstrs'
  - traverse a term, reducing all cases of abstractions applied to an arg
  - important cases are Ap and recursive constrs.
  - Ap case checks whether left reduces to an abstr before deciding what
	to do about the right arg. 

  - Example:
		(([x:A][y:B]e) a) b
		Ap (Ap (FO x (FO y e)) a) b

  - Maybe want to use f_star? this would be faster - CHECK PROFILES.
    (but, this only matters for output terms - fairly minority use...)

  - [aug02] extended to reduce things like (CCS) "subst". 
    Q: Is it worth checking that def exp is a lambda term?  

> reduceAppliedAbstrs :: [Name] -> Term -> Term
> reduceAppliedAbstrs = raa

> raa [] t@(GlobalDef{}) 
>  = t
> raa ns t@(GlobalDef n d)
>  | n `elem` ns = raa ns d	-- expand def, if marked.
>	                        -- NOTE: UNOFFICIAL delta redn
>	-- NOTE: this version doesn't reduce just the applied occurrences.
>  | otherwise   = t

> raa ns (Ap l r)
>  = case (raa ns l) of
>	FO _ _ t  -> raa ns $ substitute t r	-- if left reduces to abstr
>	DP _ _ t  -> error "RAA DP!"		-- we don't reduce these.
>	t         -> Ap t (raa ns r)		-- else apply t' to r'

> raa ns (El t)             = El (raa ns t)
> raa ns (Co f a)           = Co (raa ns f) (raa ns a)	-- keep Co in place.

> raa ns (Ext (SoftCast t ty)) 
>  = Ext $ SoftCast (raa ns t) (raa ns ty)
> raa ns (Ext (HardCast t ty)) 
>  = Ext $ HardCast (raa ns t) (raa ns ty)	-- ok, typeless reduction

> raa ns (FO v ty t)        = FO v (raa ns ty) (raa ns t)
> raa ns (DP v ty t)        = DP v (raa ns ty) (raa ns t)
> raa ns (Let v t1 t2)      = Let v (raa ns t1) (raa ns t2) 
>				-- WAS: raa $ substitute t2 t1
> raa ns t                  = t





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
FOR OLD ETA CODE - see RCS files.

`etaContract'


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Compilation of Constructor Elimination Functions. 

Since terms are closed, and have form of FO binders around a matrix of Aps and
IVars, then we can remove...   


> compileReductionAtom :: Name -> Term -> Term
> compileReductionAtom n rhs
>  = c_trace ("RECOMP ATOM = " ++ show n ++ "\n") $		-- for check.
>    F_Const n (compileReductionFn rhs)

NOTE: Seq'ing the function causes a crash for ghc-3.02


---
INTERNAL - should hide.

> compileReductionFn :: Term -> IotaReductionFn
> compileReductionFn t
>  = left `seq` 
>    left_args `seq` 
>    fn
>    where
>		(_,body) = un_FO t 
>		(left, left_args) = flattenAp body
>		fn = execute left left_args 

NOTE: bs isn't used - should we maintain this as a check??


---
NB args are in reverse (ie, correct) order for sdb_star_iota here!

> execute :: Term -> [Term] -> IotaReductionFn
> execute lt lt_as args
>  = _scc_ "execute(iota)" 
>    f_ (map (sdb_star_iota args) lt_as) 
>       (sdb_star_iota args lt)


---
Simplified subst for IotaReductionFn
 - these are closed terms, hence the direct list indexing on the IVar case
 - outside the "safe" region, we revert to sdb_star

<> {-# INLINE sdb_star_iota #-}

> sdb_star_iota :: [Term] -> Term -> Term

TODO: stats on avg call size, eg worth making to array? 

> sdb_star_iota as (IVar i) 
>  = _scc_ "sdb_star_iota:IVar" (guarded_lu "sdb_star_iota" as i) 
>  -- this is the critical case!
>  -- since the term has no leading binders, then binding depth can be ignored
>  -- and no shifting is required.

> sdb_star_iota as (Ap l r)     
>  = Ap (sdb_star_iota as l) (sdb_star_iota as r)
> sdb_star_iota as (Co l r)     
>  = Co (sdb_star_iota as l) (sdb_star_iota as r)
> sdb_star_iota as (El t)
>  = El (sdb_star_iota as t)

> sdb_star_iota as t@(Type)          = t
> sdb_star_iota as t@(Elim _ _)      = t
> sdb_star_iota as t@(F_Elim _)      = t
> sdb_star_iota as t@(Const _ _)     = t
> sdb_star_iota as t@(F_Const _ _)   = t
> sdb_star_iota as t@(ContextVar _)  = t
> sdb_star_iota as t@(GlobalDef _ _) = t
> sdb_star_iota as t@(MetaVar _)     = t

> sdb_star_iota as t@(Closed _ _)    = t
>    -- NB point of Closed is that we DO NOT need to subst in outer vars, 
>    --    (because term is closed) - so we don't need to change _anything_


Anything else, just call normal sdb_star

> sdb_star_iota as t = sdb_star_entry as t


---
Simple unbundling of the FOs.

> un_FO :: Term -> ([Binding], Term)
> un_FO (FO n ty t)
>  = ((n,ty) : bs, r)
>    where
>       (bs, r) = un_FO t

> un_FO t = ([], t)


%-------------------------------------------------------------------------------
Weaker case for arbitrary terms (eg elim args).
 - essentially, we can't use optimised replace_IDX. 

> compileArbitraryTerm :: Term -> Term

> -- new
> compileArbitraryTerm t = t 
> -- new

> compileArbitraryTerm t@(Closed _ _)   = t -- don't repeat conv.
> compileArbitraryTerm t@(F_Const _ _)  = t -- error "compile - FConst."

> compileArbitraryTerm t
#define DONT_COMPILE_ARB_TERMS 0
#if DONT_COMPILE_ARB_TERMS
>  | True
>     = t
#endif

* don't touch non-binders
* consider whnf first - yes, a useful step
* still return the whnf form, to avoid recomp. (since returning terms anyway)
   - what can be done to speed up?
* . 

>  | not_closed_term 0 t 
>     = c_trace ("compileArbitraryTerm - OPEN TERM " ++ show t ++ "\n") t
>  | otherwise 
>     = Closed t compiled_fn
>  where
>		(bs,body) = un_FO t
>		visible_arity = length bs
>		compiled_fn = compile_arbitrary_body visible_arity body



---

21aug03 - check that the seqs aren't actually hindering things.

> compile_arbitrary_body :: Arity -> Term -> ClosedReductionFn
> compile_arbitrary_body visible_arity body
>  = c_trace ("compile_arbitrary_body " ++ show visible_arity ++"\n"++ show body
>		++ "\n---\n" ++ unlines (map show $ left : left_args) ++ "\n") $
>    left `seq` 
>    left_args `seq` 
>    stack_splitter `seq` 
>    execute_arbitrary_term stack_splitter left left_args 
>    where
>	(left, left_args) = flattenAp body
>	stack_splitter    = split_stack visible_arity

---

> type StackSplitter = [Term] -> ([Term],[Term])
> split_stack :: Int -> StackSplitter
> split_stack 0 s =                              ([], s)
> split_stack 1 s = case s of (a:s)           -> ([a], s)
> split_stack 2 s = case s of (b:a:s)         -> ([a,b], s)
> split_stack 3 s = case s of (c:b:a:s)       -> ([a,b,c], s)
> split_stack 4 s = case s of (d:c:b:a:s)     -> ([a,b,c,d], s)
> split_stack 5 s = case s of (e:d:c:b:a:s)   -> ([a,b,c,d,e], s)
> split_stack 6 s = case s of (f:e:d:c:b:a:s) -> ([a,b,c,d,e,f], s)

> split_stack n s = error "need larger stack fn (split_stack)"
> -- split_stack n s = (reverse $ take n s, drop n s)


NOTE: tidy up the groupings - 

<> reReComp = c_trace "Repeat ..." - for testing purposes.

it's a stack modification thing - do subst of N args on stack 
NB these could be avoided, if args were in reverse order anyway? 
also need to separate off...


QQQ: strictness useful here?



> execute_arbitrary_term 
>  :: StackSplitter -> Term -> [Term] -> ClosedReductionFn

> execute_arbitrary_term stack_splitter lt lt_as stack
>  = f_ 
>	(_scc_ "EAT_stack" 
>         foldr (\a as  -> substitute_star args a : as) rest lt_as)
>	(_scc_ "EAT_head" 
>         substitute_star args lt)
>    where
>	(args,rest) = stack_splitter stack



QQQQ: the repl on LT - look at this - is it SUPERFLUOUS? 
	(maybe not if it is a lambda...)

stack - top N need to be reversed, and treated as args




---

> compileArbitraryDefTerm :: Term -> Term

> compileArbitraryDefTerm t@(Closed _ _)   = t -- don't repeat conv.
> compileArbitraryDefTerm t@(F_Const _ _)  = t -- error "compile - FConst."

> compileArbitraryDefTerm t
>  | not_closed_term 0 t 
>     = c_trace ("compileArbitraryDefTerm - OPEN TERM " ++ show t ++ "\n") t
>  | otherwise 
>     = c_trace ("compileArbitraryDefTerm "++show arity++" "++ show t ++"\n\t"++ 
>			(show $ viewClosedReductionFn $ compile_arbitrary_def_body body) ++ "\n") $
>       arity `seq` 
>       Closed t (compile_arbitrary_def_body body)
>  where
>		(bs,body) = un_FO t
>		arity = length bs
>		compile_arbitrary_def_body = compile_arbitrary_body arity


---

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Utils and unboxed-ness

---
XXXX what is the fastest way of doing this?

> is_negative :: Int -> Bool
> is_negative n = signum n < 0


---



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
TESTS

#if 0

> t0 = FO (bv "T") Type $ FO (bv "x") (IVar 0) $ Ap (cv "f") (IVar 0)
> t1 = FO (bv "T") Type $ FO (bv "x") (IVar 0) $ Ap (cv "f") (LocalDef 3 Type)

> t2 = FO (bv "x") (IVar 0) $ Ap (Ap (Var $ Plain "f") (IVar 1)) (Var $ Plain"y")
> t3 = FO (bv "T") Type $ FO (bv "x") (IVar 0) $ Ap (Ap (Var $ Plain "f") (IVar 1)) (Var $ Plain "y")


> --r i (IVar j) | i == j = Just $ Ap (Var $ "repl-" ++ show (i,j)) (IVar 9)
> r i (IVar j) | i == j = Just $ Ap (Var $ Plain $ "repl-" ++ show (i,j)) (LocalDef 9 Kind)
> r i _ = Nothing 

> rt = replaceTerminal

> bv = Bound . Plain
> cv = ContextVar . Plain


> cat = compileArbitraryTerm
> nct = not_closed_term


%---------------------------------------

TESTING FOR FAST REDUCTION STUFF.

> rc = compileClosedReductionFn
> e1 = Ap (IVar 0) (Ap (IVar 1) (IVar 0))
> rt1 = FO (bv "a") Type $ 
>       FO (bv "b") Type $
>       FO (bv "c") Type $
>       FO (bv "d") Type $
>       Ap e1 (Ap e1 e1)
> rt2 = rc rt1

> tx t = t args not_args
> ts t = f_ args t

> args = (map (Var . Plain . show) [1..4])
> not_args = [] -- (map (Var . Plain . show) [1..10])

NOTE: have to keep 4 args, since compilation relies on known arity. 
      Extra args should go in not_args. 

#endif
