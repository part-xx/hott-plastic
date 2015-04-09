> module Convertibility (
>				convertible,		-- with NO substitution
>				unifiable,			-- with substitution
>				test_conv_or_unify,	-- either, according to option

>				applySubstitution,
>				emptySubstitution,
>				isEmptySubstitution,
>				showSubstitution,
>				Substitution(..), OkF
>			) where


Tests if two terms are convertible.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
INFO

Conversion, as per rules. 

Maintains strict "side-ness" for the terms being converted - ie, an error
message mentioning "left" will concern a subterm derived from the left
argument.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


> import Data.Maybe
> import Data.List(insertBy)


> import Base hiding (M_)
> import Context(Ctxt, orderVars, addCtxt_BV)
> import Terms
> import SharedSyntax
> import TermOps(applyToDummy, replaceTerminal, namesIn)
> import CommandTypes(ConvOpt(..))

> import Reduction(FlatTerm(..), flatten, unflatten, Result(..), dwhnf_)
> import Reduction(reducibleApplication, applyToDummy_FlatTerm)

> import Printing(showTerm)
> import PrettyAux(shortRender, longRender, stext)
> import Pretty(Doc, text, (<+>), nest, vcat)

> import Universes(has_name_in, has_solution)

> import DebugOptions(traceOnDebugFlag)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
NOTES (to self):

Q: using continuations with reducibility tests - any speed improvements? 
BUT - need 2 extra args for pass and fail ctus - plus problems of arg permute?





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
TRACING and DEBUGGING functions.

Use switchable, forced trace.

> conv_trace :: Ctxt -> String -> FlatTerm -> FlatTerm -> a -> a
> conv_trace c m lt rt
>  = let msg = conv_report c m lt rt
>    in traceOnDebugFlag "CONV_TRACING" (msg `fseq` msg)

---
`conv_report'
  - neat layout of terms, shown in context

> conv_report :: Ctxt -> String -> FlatTerm -> FlatTerm -> String
> conv_report c m lt rt
>  = m ++                                                         "\n" 
>      ++ (shortRender $ nest 4 $ text "LT" <+> show_ (unflatten lt))++"\n"
>      ++ (shortRender $ nest 4 $ text "RT" <+> show_ (unflatten rt))++"\n"
>    where
>		-- show_ = text . show		-- for debugging.
>		show_ = showTerm c


---
`unify_trace'
  - report the results of unification
  - use forced, switchable traces.

> unify_trace c ok@(Ok [])
>  = unify_trace_fseq ("UNIFIED, with EMPTY substitution\n") ok
> unify_trace c ok@(Ok ss)
>  = unify_trace_fseq ("UNIFIED, with substitution:\n" ++ showSubstitution c ss) ok
> unify_trace c ok@(Fail m)
>  = unify_trace_fseq ("COULDN'T UNIFY: because " ++ m ++ "\n") ok

> unify_trace_fseq m = traceOnDebugFlag "UNIFY_TRACING" (m `fseq` m)



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
MAIN (EXTERNAL) FUNCTIONS.

`convertible' 
  - Ok or Fail test on conversion without unification.

> convertible :: Ctxt -> Term -> Term -> OkF ()
> convertible c t1 t2 
>  = do
>		test_conv_or_unify StrictConv c t1 t2
>		return ()


---
`unifiable' 
  - run conversion with view to producing a substitution.

> unifiable :: Ctxt -> Term -> Term -> OkF Substitution
> unifiable c t1 t2 = unify_trace c $ conv c t1 t2



---
`test_conv_or_unify'
  - intended for implementing a testing command; returns String or Fail.
  - if strict conv. chosen, then checks that conversion doesn't produce a 
	substitution.
  - if unify chosen, then shows the result nicely.

> test_conv_or_unify :: ConvOpt -> Ctxt -> Term -> Term -> OkF String
> test_conv_or_unify StrictConv c t1 t2
>  = do
>		ss <- conv c t1 t2
>		if isEmptySubstitution ss
>		  then
>			return "Converts ok\n"
>		  else
>			fail_with_msg $ "Conversion resulted in unification:\n" 
>								++ showSubstitution c ss

> test_conv_or_unify UnifyConv c t1 t2
>  = do
>		ss <- unifiable c t1 t2
>		case ss of
>			[] -> return "Unifies, no substitution required"
>			ss -> return $ "Unifies, with substitution:\n" ++ showSubstitution c ss




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
`conv' 
  - convertibility for two terms.
  - calls `cvt' after flattening the two terms.

> conv :: Ctxt -> Term -> Term -> M
> conv ctxt t1@(Co c a) t2@(Ap m@(MetaVar _) x)
>  = conv_trace ctxt "Conv (CO case) testing:" flatten_t1 flatten_t2 $ 
>    conv_list ctxt [c,a] [m,x]
>    where
>		flatten_t1 = flatten t1
>		flatten_t2 = flatten t2

IDEA: allow higher order unif. in this highly constrained case.
      there's a requirement that co solutions are ONLY co.


> conv c t1 t2
>  = conv_trace c "Conv testing:" flatten_t1 flatten_t2 $ 
>    cvt c flatten_t1 flatten_t2
>    where
>		flatten_t1 = flatten t1
>		flatten_t2 = flatten t2


%---------------------------------------
`cvt' 
  - does all the work, and calls `conv' to compare subterms.
  - cases are handled in the series of sections below (after the monad)

> cvt :: Ctxt -> FlatTerm -> FlatTerm -> M



%-------------------------------------------------------------------------------
The monad

 * need useful fail
 * need changeable context, for right context and printing, plus metavars.
 * might need to return a substitution for Unify.

BUT: passing context explicitly since this simplifies handling of the
backtracking cases.

Hence the following:

> type M_ a = OkF a
> type M = M_ Substitution

---
`conv_ok' - synonym for success. 
`conv_if' - lifts boolean to monad. 
		  - NB, the uninformative error msg is often superceeded by caller.

> conv_ok :: M
> conv_ok = return emptySubstitution

> conv_if :: Bool -> M
> conv_if True  = conv_ok
> conv_if False = fail_with_msg "conv_if test failed"

---
`conv_FAIL' - synonym for (big) failure.

> conv_FAIL c m l r
>  = fail_with_msg $ conv_report c m l r



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
CVT: SPECIFIC UNIMPLEMENTED

eg, do we fail if we see a Let? 
	(mar99) let handled via reducibleApplication...

<NOTHING> - and rely on compiler trapping.


%---------------------------------------


<> cvt c (E (CAST le lt) []) (E (CAST re rt) [])
<>  = conv c le re
	DISABLE - should not allow unguarded recurse in to CAST term? 
	        - Following case does better job.

<> cvt c l@(E (Ext (HardCast le lt)) ls) r@(E (Ext (HardCast re rt)) rs)
<>  = if eqConst le re && isOk args_convert
<>		then do
<>				-- no substit here, aim for simple cases.
<>				-- ss <- conv c lt rt		-- types must convert.
<>				-- sss <- conv_list c ls rs
<>				-- return $ combineSubstitutions c ss sss
<>				-- no - make use of NAME EQUIVALENCE.
<>				-- IMPORTANT - better to ensure this via ctxt
<>				args_convert {- PASS -}
<>		else do
<>			let r_left  = reducibleApplication l
<>			let r_right = reducibleApplication r
<>			error "not considered CAST cases yet"

<>			case r_right of
<>				Nothing -> case r_left of 
<>								Nothing -> conv_FAIL c fail_msg l r
<>								Just l2 -> cvt c l2 r
<>				Just r2 -> case r_left of 
<>								Nothing -> cvt c l  r2
<>								Just l2 -> cvt c l2 r2

<>    where
<>		fail_msg = "CAST terms not convertible"
<>		args_convert = conv_list c ls rs



IMPORTANT: this just catches simple cases of equality, eg (T^x ls) (T^x ys)
	need to think more about this. Eg, significance of types converting? 

THIS RAISES ISSUE OF autogen name equality, (ie, from (T^n ?)).
might be a problem; we want extensional equality - but conv can recurse inside
the T^n. . 

<WAS>  = error "asym CAST cmp"

%---------------------------------------
eg T^i ? and something.

******************
** ALL DISABLED **
******************

IMPL does not implement the full restrictions proper

want to abstract univ repres. details from here, eg Universes.has_solution...

<> cvt c l@(E (CAST _ _) _) r@(E (El _) _)	-- avoid getting El in solutions
<>  = conv_FAIL c "Cast vs (El x)" l r
<> cvt c l@(E (El _) _) r@(E (CAST _ _) _)	-- best to do here? 
<>  = conv_FAIL c "(El x) vs CAST" l r

******************
** ALL DISABLED **
******************



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
first step to metavars

> cvt c left right
>  | isOk mv_result = mv_result
>    where
>	mv_result = cvt_mv c left right



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
CVT: Both are El-applications

El terms must convert directly - ie, since El(x) is a kind, then no reducible 
term will yield it.

Kinds also can't be applied, so we only need to check the no-args case.

Complain (ie, crash) if we find an El which is applied to arguments.

> cvt c (E (El l) []) (E (El r) [])
>  = conv c l r

> cvt c l@(E (El _) _) r
>  = conv_FAIL c "Comparing El to non-El" l r
> cvt c l              r@(E (El _) _)
>  = conv_FAIL c "Comparing non-El to El" l r

> cvt c left@(E (El _) _) right@(E (El _) _) 
>  = error $ 
>    conv_report c "Comparing El terms that have arguments" left right
>    ++ "\n\n *** POSSIBLE INTERNAL TYPE-SYNTHESIS ERROR? ***\n"




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
CVT: BOTH are Application Expressions.

NB - ignore arities of specific things like Type 
   - TypeInference should catch them.
   - MetaVar cases are handled above.

METHOD:
  - do the terms have the same (identical) head constant?
	 - do they have the same number of arguments? 
		- do the arguments pairwise convert? 
		   - THEN return what the substitution is.
  - OTHERWISE
	 - call try_right_then_left, passing sufficient arguments to allow that
	   function to produce an interesting error message.

NOTE:
 - there is no substitution arising from comparison of head constants.
 - all treatment of metavariables-with-arguments is handled above.

 - does forcing the length and using it as discriminator make a difference? 
   arguably it is cheaper than (min m n) conversions which may fail. 
   (Is there a case where collecting arguments is expensive? unlikely...)

 - Having the same head does NOT mean that both terms are reducible 
   (eg E_ (f x) 0 and E_ (g 0) x, where x is a var.)

 - some error functions accumulate error messages from below, thus showing
   how the conversion failed and the path(s) reduction took.

%---------------------------------------
need refine here.

to dwhnf, -> either elim-var or constr head 
at this point, can compare for same term in low level, eg both elims.

%---------------------------------------
This case needed for T^n a = T^n b equations

> cvt c  left@(E (Ext (HardCast le lt)) ls) 
>       right@(E (Ext (HardCast re rt)) rs)
>  | eqConst le re 
>     = case conv_list c ls rs of
>	  Fail _  -> fail_ "CAST terms not convertible"
>	  ok@Ok{} -> ok
>  where
>		fail_ m = conv_FAIL c m left right


%---------------------------------------
Next 2 cases for T^n ? = T equations

WARNING
  has_solution test doesn't implement internal (non-parameter) restrictions 
  from the constructor schemata - so is unsound. 

  May look at it again some time (the code exists via AddToUniverse etc).

WEAKNESS
  This looks for universe decoders purely by finding the cast
  This isn't satisfactory (although has_solution will catch)

DISABLED 6aug03 - too many problems. better to internalise the indices.

<> cvt c l@(E lc@(Ext (HardCast{})) [MetaVar la]) r@(E _ _)
<>  = do
<>		conv_trace c ("CAST: " ++ show lc ++ "\n") l r $ return ()
<>		t <- embed $ has_solution lc (unflatten r)
<>		mk_subst la t

<> cvt c l@(E _ _) r@(E rc@(Ext (HardCast{})) [MetaVar ra])
<>  = do
<>		conv_trace c ("CAST: " ++ show rc ++ "\n") l r $ return ()
<>		t <- embed $ has_solution rc (unflatten l)
<>		mk_subst ra t


%---------------------------------------
Checking prf terms

> cvt c left@(E (ContextVar (Plain "Prf")) [l]) right@(E (ContextVar (Plain "Prf")) [r]) 
>  = conv c l r 

%---------------------------------------

 * if equal set of symbols, go no further
 * if unifiable in parts, then loop back on updated terms
 * else try expand until block.

> cvt c left@(E l ls) right@(E r rs) 
>  | same_shape && and (zipWith eqConst ls rs)
>  = conv_ok

>  | same_shape 
>  = conv_trace c "Pre-easy unifs" left right
>  $ case conv_mv_list c ls rs of
>	Right ss -> -- perfect match (on simple consts)
>	            conv_trace c "Easy unifs matches to:" 
>		      (applySubstitution ss left) (applySubstitution ss right)
>		  $ return ss

>	Left ss  -> -- imperfect match, needs more work
>	            let new_left  = applySubstitution ss left
>	                new_right = applySubstitution ss right in
>	            conv_trace c "Easy unifs PARTIAL match:" new_left new_right
>		  $ do
>		      sss <- cvt_blocked c (dwhnf_ new_left) (dwhnf_ new_right)
>		        -- IMPORTANT: have to go to blocked stage now
>	              return $ combineSubstitutions c ss sss

>  | otherwise
>  = conv_trace c "Trying cvt on blocked terms" blocked_left blocked_right
>  $ cvt_blocked c blocked_left blocked_right
>    where
>	same_shape = eqConst l r && sameLength ls rs
>	blocked_left  = dwhnf_ left
>	blocked_right = dwhnf_ right


%-------------------------------------------------------------------------------
CVT: catch-all case for comparing E's - shouldn't be reached.

> cvt c left@(E _ _) right@(E _ _) 
>  = error $ 
>    conv_report c "hit E~E default case. shouldn't!" left right

WAS:
conv_FAIL c "Application cases don't convert" left right


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
CVT: Comparing Expr + Abstr

METHOD
 - If abstr is not a FO, then try expanding the Expr (if possible) and
   try conversion with that.

 - If Abstr is a FO then use the Coquand trick for eta conversion. 
   Instead of (expensively) testing the body of the abstr to see if it 
   has form (B x), where x is the bound var and B is x-free, and then testing
   convertibility with B, we apply the other Expr term to a dummy var so that
   it ITSELF looks like an abstr body in (B x) form, and test this against the 
   body of the abstraction. Ie, it has effect:
	 naive: ([x:T]B x) ~ E  => B ~ E
	 trick: ([x:T]C)   ~ E  => C ~ applyToDummy E

   If the new conversion fails, then we fail without reducing the Expr side.
   This is because the eta trick will cause the Expr side to be reduced
   in the course of comparison. 

   NB we add a binder to the context when comparing the bodies.

IMPL
 - We keep the cases separate (instead of eg implementing A ~ E by swapping
   order of arguments and recursing) in order to help debugging - ie the left
   arg is ALWAYS the left arg...
 - OBVIOUSLY - need to make sure the two clauses are symmetric! 

 - saving a bit of time by applyToDummy_FlatTerm, rather than applyToDummy
   with unflattening etc.

%-------------------
FO cases - can do eta trick.

> cvt c left@(E _ _) right@(A ra@(FO n ty body))
>  = conv_trace c "Conv E A: TRY ETA" left (flatten ra) $
>    case try_eta of							-- can only try eta on a FO.
>    Ok ss  -> {-PASS-} try_eta
>    Fail m -> case reducibleApplication left of
>              Blocked _ -> {-FAIL-} couldn't_convert_expr_and_abs
>              Reduced _  -> {-FAIL-} couldn't_convert_REDUCIBLE_expr_and_abs
>    where
>		flat_body = flatten body
>		try_eta = cvt (addCtxt_BV n ty c) (applyToDummy_FlatTerm left) flat_body
>
>		couldn't_convert_expr_and_abs
>		 = fail_couldn't_convert "Expr" "FO Abs" c left right
>		couldn't_convert_REDUCIBLE_expr_and_abs
>		 = fail_couldn't_convert "REDUCIBLE Expr" "FO Abs" c left right


> cvt c left@(A la@(FO n ty body)) right@(E _ _)
>  = conv_trace c "Conv A E: TRY ETA" (flatten la) right $
>    case try_eta of							-- can only try eta on a FO.
>    Ok ss  -> {-PASS-} try_eta
>    Fail m -> case reducibleApplication right of
>              Blocked _ -> {-FAIL-} couldn't_convert_abs_and_expr
>              Reduced _ -> {-FAIL-} couldn't_convert_abs_and_REDUCIBLE_expr
>    where
>		flat_body = flatten body
>		try_eta = cvt (addCtxt_BV n ty c) flat_body (applyToDummy_FlatTerm right) 
>
>		couldn't_convert_abs_and_expr
>		 = fail_couldn't_convert "FO Abs" "Expr" c left right
>		couldn't_convert_abs_and_REDUCIBLE_expr
>		 = fail_couldn't_convert "FO Abs" "REDUCIBLE Expr" c left right

%-------------------
DP cases - nothing can produce a DP, so fail here.

> cvt c left@(E _ _) right@(A (DP _ _ _))
>  = fail_couldn't_convert "Expr" "DP Abs" c left right

> cvt c left@(A (DP _ _ _)) right@(E _ _)
>  = fail_couldn't_convert "DP Abs" "Expr" c left right



%-------------------
Catch remaining cases.

> cvt c left@(E _ _) right@(A _)
>  = error $ 
>    conv_report c "hit E~A default case. shouldn't!" left right

> cvt c left@(A _) right@(E _ _)
>  = error $ 
>    conv_report c "hit A~E default case. shouldn't!" left right


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
CVT: Comparing Abstractions.

 - the types and the bodies must be convertible.
 - when processing the bodies, do so in a context that contains the bound var. 
 - the type of this new var is (arbitrarily) from the left term (NB, this type 
   is converible with the right, if the conv of bodies is reached, of course)
 - the new var name is a mixture of the two names. (MIGHT REMOVE.)

> cvt c (A (DP v1 ty1 t1)) (A (DP v2 ty2 t2))
>  = do
>		s1 <- conv c ty1 ty2 
>		s2 <- conv	(addCtxt_BV new_var ty1 c) 
>					(applySubstitution s1 t1)
>					(applySubstitution s1 t2)
> 		return $ combineSubstitutions c s1 s2
>    where
>		new_var | v1 == v2  = v1
>		        | otherwise = Bound $ Plain $ show v1 ++"/"++ show v2

> cvt c (A (FO v1 ty1 t1)) (A (FO v2 ty2 t2))
>  = do
>		s1 <- conv c ty1 ty2 
>		s2 <- conv	(addCtxt_BV new_var ty1 c) 
>					(applySubstitution s1 t1)
>					(applySubstitution s1 t2)
> 		return $ combineSubstitutions c s1 s2
>    where
>		new_var | v1 == v2  = v1
>		        | otherwise = Bound $ Plain $ show v1 ++"/"++ show v2

> cvt c l@(A _) r@(A _)
>  = conv_FAIL c "Mismatched binders" l r



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Nothing else works.

BUT - we should have tested and caught the possibility above. 
	- therefore, we crash here.

> cvt c l r
>  = error $ conv_report c "Reached Catch-all case, should not occur." l r



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

> cvt_blocked c left@(E l ls) right@(E r rs) 
>  | eqConst l r
>     = case have_same_length of
>             True  -> -- arg count is the same.
>                      case args_convert of
>                      Ok _   -> {-PASS-} args_convert
>                      Fail _ -> -- args do not convert.
>                                fail_ "args differ"
>                                -- failure here, eg eg Cons a b /~ Cons c d

>             False -> -- arg counts differ.
>                      -- NOTE: failure here should be caught by type_checking ?
>                      -- eg Cons a b c /~ Cons a b
>                      error "Arg mismatch under same heads"
>    where
>	have_same_length = sameLength ls rs
>	args_convert = conv_list c ls rs
>	fail_ m = conv_FAIL c m left right

> cvt_blocked c  left@(E (Ext (HardCast le lt)) ls) 
>               right@(E (Ext (HardCast re rt)) rs)
>  | eqConst le re 
>     = case conv_list c ls rs of
>	  Fail _  -> fail_ "CAST terms not convertible"
>	  ok@Ok{} -> ok
>  where
>		fail_ m = conv_FAIL c m left right

> cvt_blocked c left@(E l ls) right@(E r rs) 
>  = -- non-equal heads 
>    -- failure here, eg (Cons x y) vs (Nil).
>    case cvt_mv c left right of
>	    ok@Ok{} -> ok
>	    Fail _  -> fail_ "head symbols differ or not unifiable"
>    where
>	fail_ m = conv_FAIL c m left right

> cvt_blocked c l@(A{}) r	-- pass back to main cvt
>  = cvt c l r
> cvt_blocked c l r@(A{})	-- pass back to main cvt
>  = cvt c l r

> cvt_blocked c l r
>  = conv_FAIL c "cvt_blocked for:" l r



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
CVT: METAVAR CASES.

NB here, we can do tests about solvability (eg (B a) vs (? ? a))

NB same metavar test - should be here.
<>		eq (MetaVar i1)     (MetaVar i2)     = conv_if $ i1 == i2
ANS: it is handled via orderVars returning Eq.
	 (if speed important >> error checks, can weaken orderVars; see src)

%-------------------------------------------------------------------------------
SIMPLE METAVAR CASES. 

 - Just for unapplied terminals.
 - No equational unification (eg (B a) and (? b a))
 - Substitutions are ordered so that older vars get substituted with the younger

> cvt_mv c (E left_var@(MetaVar left) []) (E right_var@(MetaVar right) [])
>  | left == right	-- avoid dbl lookup if avoidable
>  = conv_ok
>  | otherwise 
>  = do
>	order <- orderVars c left right		-- get order wrt context
>	case order of
>		LT -> mk_subst right left_var   -- r < l, replace n2
>		GT -> mk_subst left  right_var  -- l > r, replace n1
>		EQ -> error "identical MVs should be picked up earlier"
 
> cvt_mv c l@(E (MetaVar left) ls) r@(E (MetaVar right) rs)
>  | left == right 
>     = if length ls == length rs && and (zipWith eqConst ls rs)
>	then conv_ok
>	else conv_FAIL c fail_lists l r
>    where
>	fail_lists = "Arguments lists under identical MV heads don't match"

<>	-- eg (?1 ?2) = (?1 ?2)
<>	-- BUT (?1 x) (?1 ?2) = unsafe; so mvs only!
<>	-- might reconsider this later.

PREVIOUS VERSIONS.
<>     -- expensive !!! EQ -> conv_list c ls rs-- EXPERIMENT.
<>     -- = conv_mv_list c ls rs		-- EXPERIMENT.
HERE - to decide - keep or no? 
     - this postpones full unif check until later
     - might be cheapr in the long run.

 
> cvt_mv c l r@(E (MetaVar _) (_:_))
>  = conv_FAIL c "Unification on Metavar-headed terms NYI" l r
  
> cvt_mv c l@(E (MetaVar _) (_:_)) r
>  = conv_FAIL c "Unification on Metavar-headed terms NYI" l r


> cvt_mv c l (E (MetaVar r) [])
>  = mk_subst r $ unflatten l
  
> cvt_mv c (E (MetaVar l) []) r
>  = mk_subst l $ unflatten r

> cvt_mv c _ _ 
>  = Fail "not a metavar problem"




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
`try_right_then_left'
  - when comparing E's, try reducing first the right then the left term, to 
	see if a conversion is possible with the reduced form.

  - if the right reduces but no conversion, then try reducing left. if ok
	then try conv with
	   a) BOTH reduced
       b) just the LEFT reduced? 
    not sure - no difference, just a question of efficiency vs preservation? 

<> try_right_then_left :: E_Cvt_Error -> (String -> M) -> Ctxt -> FlatTerm -> FlatTerm -> M
<> try_right_then_left error_case error_fn c left right
<>  = case reducible_right of
<>    Nothing    -> case reducible_left of
<>                  Nothing    -> {-FAIL-} error_fn "Couldn't reduce either side"
<>                  Just lEFT  -> let  cvt_left = cvt c lEFT right in
<>                                case cvt_left of
<>                                Ok _   -> {-PASS-} cvt_left
<>                                Fail _ -> {-FAIL-} reducible_won't_convert "left" error_case $
<>                                                   cvt_left
<
<>    Just rIGHT -> let cvt_right = cvt c left rIGHT in
<>                  case cvt_right of
<>                  Ok _   -> {-PASS-} cvt_right 
<>                  Fail _ -> case reducible_left of 
<>                            Nothing   -> {-FAIL-} reducible_won't_convert "right" error_case $
<>                                                  cvt_right
<>                            Just lEFT -> let cvt_left = cvt c lEFT right in
<>                                         case cvt_left of
<>                                         Ok _   -> {-PASS-} cvt_left
<>                                         Fail _ -> {-FAIL-} reducible_won't_convert "left after right fail" error_case $
<>                                                            cvt_left
<>    where
<>		reducible_left  = reducibleApplication left
<>		reducible_right = reducibleApplication right
<
<>		reducible_won't_convert :: String -> E_Cvt_Error -> M -> M
<>		reducible_won't_convert "both" what
<>		 = prepend_msg $ conv_report c msg left right
<>		   where msg = "Reducible terms (both) aren't convertible " ++ decode_ec_error what
<>		reducible_won't_convert which what
<>		 = prepend_msg $ conv_report c msg left right
<>		   where msg = "Reducible term ("++which++") isn't convertible "++decode_ec_error what


---
`E_Cvt_Error'
  - represent case of error in conversion of E terms.
  - meaning represented by `decode_ec_error'

> data E_Cvt_Error = Parity | Lengths_differ | Neq_heads

> decode_ec_error Parity         = "(identical head, arg counts same)"
> decode_ec_error Lengths_differ = "(identical head, arg counts differ)"
> decode_ec_error Neq_heads      = "(different heads)"


---
`fail_couldn't_convert'
  - generic fail function.

> fail_couldn't_convert 
>  :: String -> String -> Ctxt -> FlatTerm -> FlatTerm -> M
> fail_couldn't_convert lm rm c left right
>  = conv_FAIL c msg left right
>    where
>		msg = unwords ["Could not convert", lm, "with", rm]



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
`conv_list'
  - checks that terms are pairwise convertible
  - substitutions are propagated through the list as they are generated.
  - the caller should check list length first
  - but for safety, we assert that recursion ends on double empty lists.
	(ie, no zipWith!)

> conv_list :: Ctxt -> [Term] -> [Term] -> M
> conv_list c [] [] 
>  = conv_ok
> conv_list c (l:ls) (r:rs)
>  = do
>		ss <- conv c l r
>		sss <- conv_list c 
>						 (map (applySubstitution ss) ls) 
>						 (map (applySubstitution ss) rs)
>		return $ combineSubstitutions c ss sss

> -- catch and fail here.
> conv_list c ls@[] rs = conv_list_error c ls rs
> conv_list c ls rs@[] = conv_list_error c ls rs




%---------------------------------------
unifying a list of terms (under identical heads)

rebuilding
 - how to get unified term out ? subst applied to EITHER side
 - there's an efficiency question too? 
 - easy - unify then subst. 
 - NB note this ignores mismatches, so have to subst BOTH terms.
 - LEAVE THIS FOR LATER. (not impl yet)

Right = unifying ok
Left  = some disagreement found.

> conv_mv_list :: Ctxt -> [Term] -> [Term] -> Either Substitution Substitution

> conv_mv_list c [] [] 
>  = Right emptySubstitution

> conv_mv_list c (l:ls) (r:rs)
>  | eqConst l r
>     = conv_mv_list c ls rs

>  | otherwise
>  = case cvt_mv c (flatten l) (flatten r) of
>	Fail _ -> toLeft $ conv_mv_list c ls rs
>	Ok ss  -> addIn (combineSubstitutions c ss) 
>	        $ conv_mv_list c (map (applySubstitution ss) ls) 
>			         (map (applySubstitution ss) rs)
>  where
>	toLeft l@(Left _) = l		-- propagate
>	toLeft (Right s)  = Left s	-- signal incomplete result
>	addIn f (Left s)  = Left (f s)
>	addIn f (Right s) = Right (f s)

> -- catch and fail here.
> conv_mv_list c ls@[] rs = conv_list_error c ls rs
> conv_mv_list c ls rs@[] = conv_list_error c ls rs


%---------------------------------------
Shared error msg

> conv_list_error c ls rs 
>  = do
>	let left  = text "Left " <+> vcat (map (showTerm c) ls)
>	    right = text "Right" <+> vcat (map (showTerm c) rs)
>	    msg   = text "Mismatch of arg. list lengths in applications"
>	error $ longRender $ vcat [msg, left, right]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
`eqConst'
  - basic syntactic test on constants

> eqConst :: Term -> Term -> Bool
> eqConst Type             Type             = True
> eqConst (IVar i1)        (IVar i2)        = i1 == i2
> eqConst (ContextVar i1)  (ContextVar i2)  = i1 == i2
> eqConst (MetaVar i1)     (MetaVar i2)     = i1 == i2	-- special case. 
> eqConst (GlobalDef i1 _) (GlobalDef i2 _) = i1 == i2

-- ELIM 

> eqConst (Elim i1 _)      (Elim i2 _)      = i1 == i2
> eqConst (F_Elim i1)      (F_Elim i2)      = i1 == i2
> eqConst (Elim i1 _)      (F_Elim i2)      = i1 == i2
> eqConst (F_Elim i1)      (Elim i2 _)      = i1 == i2

-- CONSTRUCTORS 

> eqConst (Const i1 _)   (ContextVar i2)    = i1 == i2
> eqConst (F_Const i1 _) (ContextVar i2)    = i1 == i2	-- needed for compile

>		-- TEST - for adding comp rules. CHECK THIS AGAIN!
>		-- DISABLED - eq (ContextVar i1)  (Const i2 _ _)   = i1 == i2

> eqConst (Const i1 _)     (Const i2 _)     = i1 == i2
> eqConst (F_Const i1 _)   (F_Const i2 _)   = i1 == i2

> eqConst (F_Const i1 _)   (Const i2 _)     = i1 == i2		-- different forms
> eqConst (Const i1 _)     (F_Const i2 _)   = i1 == i2

CLOSED TERMS - probably not reliable - they should be reduced! 
NB - should come out when being flattened.

> eqConst l@Closed{} _          = error $ "Closed left  term: " ++ show l
> eqConst _          r@Closed{} = error $ "Closed right term: " ++ show r

-- ALLOW KINDS, for new metavar code.

<> eqConst Kind     _        = error $ "Kind in Conv (left)"
<> eqConst _        Kind     = error $ "Kind in Conv (right)"

> eqConst Kind     Kind     = True


-- ASSERTIONS - should not happen.

> eqConst (Var i1) _        = error $ "Var in Conv (left) "  ++ show i1
> eqConst _        (Var i2) = error $ "Var in Conv (right) " ++ show i2

> -- failure case
> eqConst _ _ = False






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
SUBSTITUTIONS

> type Substitution = [(Name,Term)]

> emptySubstitution   = []   :: Substitution
> isEmptySubstitution = null :: Substitution -> Bool

> showSubstitution :: Ctxt -> Substitution -> String
> showSubstitution c ss
>  = longRender (vcat $ map show_one ss) ++ "\n"
>    where
>		show_one (n,t) = nest 4 $ stext n <+> text "->" <+> showTerm c t

---

> empty_subst :: Monad m => m Substitution
> empty_subst = return emptySubstitution

---
occurs check here.

> mk_subst :: (Monad m, Fallible m) => Name -> Term -> m Substitution
> mk_subst n t 
>  | n `elem` namesIn t 
>     = fail_with_msg $ "occurs check, " ++ show n ++ " " ++ show t
>  | otherwise          
>     = return [(n,t)]



%---------------------------------------
`applySubstitution'
  - replace metavars with fixed terms.
  - ASSUME that subst is in age-order for vars. 
  - NB this rewrites terms O(num of substs) - clearly bad! 
  - should rewrite to be O(1), but be caseful of [] case.

> class Substitutable t where
>    applySubstitution :: Substitution -> t -> t

> instance Substitutable Term where
>	applySubstitution ss t = foldl replace_ t ss

> instance Substitutable FlatTerm where
>	applySubstitution ss (A t)    = A (applySubstitution ss t)
>	applySubstitution ss (E t ts) = E (applySubstitution ss t)
>	                                  (map (applySubstitution ss) ts) 

> replace_ :: Term -> (Name,Term) -> Term
> replace_ t n_t = replaceTerminal (replace_metavar n_t) t

> replace_metavar (m,t) (MetaVar n)           
>  | m == n = Just t
> replace_metavar (m,t) (Var (Plain ('?':n))) 
>  | m == Plain ("m_" ++ n) = Just t			-- HACK
> replace_metavar (m,t) _ = Nothing



%---------------------------------------
`combineSubstitutions'
  - merge substitutions from 2 trees.
  - first, apply left substs to right substs
  - and then merge in var order, according to predicate.

BUT - shouldn't these already be done by caller of this function, through
      applying left substs to right terms BEFORE trying cvt? 
TRY ASSERTION.

NOTE:
 - special case for empty right subst - as will result from `conv_list'

> combineSubstitutions :: Ctxt -> Substitution -> Substitution -> Substitution
> combineSubstitutions c left []
>  = left
> combineSubstitutions c left right
>  | cmp_show = error $ "combineSubstitutions assert failed.\n" 
>			++ "Left\n"  ++ showSubstitution c left
>			++ "Right\n" ++ showSubstitution c right
>			++ "\n---\n" ++ show right 
>			++ "\n---\n" ++ show right_2 ++ "\n\n"
>  | otherwise = foldl (flip $ insertBy order) right_2 left
>    where
>	right_2 = [ (n, applySubstitution left t) | (n,t) <- right ]
>	order (v1,_) (v2,_) = elimOk (error) (id) $ orderVars c v1 v2

>	cmp_show = show right /= show right_2


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

