> module SimpleToTerm (module SimpleToTerm, SimpleTerm, SimpleType) where

Raw Terms to Term type used in calculation.

> import IOExts_(trace)

> import Data.Maybe
> import Data.Char (isAlpha, isDigit)

> import Base
> import SimpleTerm

> import Terms
> import TermOps (addDeBruijnIndices)
> import Context

<> import UserSyntax(removeUserSyntax)

> import SharedSyntax

> import Case(translateCase)

> import ContextOps(univLookup)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

> translate :: Ctxt -> SimpleTerm -> OkF Term
> translate c t
>  = c_run c $ t_ t

> translate_db :: Ctxt -> SimpleTerm -> OkF Term
> translate_db c t
>  = do
>		t2 <- return t
>		t3 <- translate c t2
>		return (addDeBruijnIndices t3)


---
`add_binder' uses NON-official add_ctxt, since we are just checking scope of
names here. (ie renaming isn't needed). 

> add_binder :: BoundName -> Ctxt_entry -> CERM a -> CERM a
> add_binder Underscore ce = change (add_ctxt (Plain "_") ce)
> add_binder (Bound i)  ce = change (add_ctxt i ce)


%---------------------------------------


> t_ :: SimpleTerm -> CERM Term

> t_ (Name_S (Plain "Type"))
>  = return Type

> t_ (Name_S (Plain "Prop"))
>  = return $ ContextVar $ Plain "Prop"

> t_ (Name_S (Plain "Prf"))
>  = return $ ContextVar $ Plain "Prf"

> t_ (Ap_S (Name_S (Plain "El")) s)
>  = do 
>		s' <- t_ s
>		return (El s')

> t_ (Ext_S e@(SoftCast _ _))
>  = fmap Ext $ mapM_Extension t_ e

> t_ (Ext_S e@(HardCast _ _))
>  = fmap Ext $ mapM_Extension t_ e

---

> t_ (Ap_S l r) 
>  = do 
>		l' <- t_ l
>		r' <- t_ r
>		return (Ap l' r')

> t_ (Pars_S t) 		-- drop the marker for parentheses
>  = t_ t

---

HACK for handling -> arrows, DISABLED for user-syntax experiments.
Re-enabled - though some libs have been modified.

> t_ (Op_S l op@(Name_S (Operator "->")) r)
>  = do
>		r' <- t_ r 
>		translate_dp_arrow l r'

---

NB extra check for field being simple name. 
#define HACKED_DOT 0
#if HACKED_DOT
> t_ (Op_S l op@(Name_S (Operator ".")) r)		-- TEMPORARY! during transition
>  = do 
>		l' <- t_ l
>		r' <- t_ r
>		return $ Ap r' l'
#endif

> t_ (Op_S l op@(Name_S (Operator ".")) r@(Name_S r_nm))
>  = do
>		l' <- t_ l
>		r' <- t_ r
>		return (Ap r' l')	-- lego-style postfix fns.

DISABLED: record syntax.
<> t_ (Op_S l op@(Name_S (Operator op_nm)) r@(Name_S r_nm))
<>  = do
<>		l'  <- t_ l
<>		case op_nm of 
<>			"." -> return $ Ext $ Project  l' r_nm [] nyi
<>			"|" -> return $ Ext $ Restrict l' r_nm []
<>			_   -> do { op' <- t_ op ; r' <- t_ r ; return (Ap (Ap op' l') r') }
<>    where
<>		nyi = Nothing -- Ext (HardCast (Var $ Plain "EXT_NYI_2") Type)

> t_ (Op_S l op r)
>  = do
>		l'  <- t_ l
>		op' <- t_ op
>		r'  <- t_ r
>		return (Ap (Ap op' l') r')


---

> t_ (Let_S n t e)
>  = do 
>		t' <- t_ t
>		e' <- add_binder (Bound n) 
>		                 (CE (Let_ t') (ce_dummy_type "STT Let")) $ 
>		      t_ e
>		return (Let n t' e')


%-------------------------------------------------------------------------------
-- might put DP, FO under Bd_S
-- BUT internally want to separate them...

> t_ (Bind_S By_DP n ty t) = t_bind DP n ty t
> t_ (Bind_S By_FO n ty t) = t_bind FO n ty t

> t_ (Bind_S (By_Brackets "{" "}") n ty t)
>  = t_ (Bind_S (By_Constr "FA") n ty t)

%-------------------------------------------------------------------------------

> t_ (Bind_S (By_Constr tc) n ty t)
>  = t_ $ Ap_S (Ap_S (Name_S $ Plain tc) ty)
>              (Bind_S By_FO n ty t)
>       -- PROVISIONAL.
>       -- (how should this be done - adding to the tables?)
>       -- sugar for binding forms, eg Pi[x:A]t ---> ((Pi A) ([x:A]t))

> t_ bd@(Bind_S bv n ty t)
>  = fail_with_msg $ 
>    "translate found Bd_S: likely that " ++ show bv ++ " is undefined.\n" ++
>    "\t" ++ show bd ++ "\n"

WAS <>    t_bind (foo bv) n' ty t
<>    where foo translated bv action.
<>    where 
<>		n' = binderName n
  

%-------------------------------------------------------------------------------
Name_S - complex - need to read from context, and maybe uniquify.
put here - depd on actions in prev cases.

> t_ (Name_S (Plain ""))
>  = fail_with_msg "Illegal - empty name" 
>							-- lexer should have caught this already? 

---
META VARIABLES
 - these handled elsewhere

> t_ (Name_S s@(Plain ('?':_)))
>  = return $ Var s

---

> t_ (Name_S n@(Plain s))
>  | not (null s) && not (isAlpha $ head s)
>     = fail_with_msg $ "something wrong with id:" ++ s ++ ":"
>  | otherwise
>     = categorise n

> t_ (Name_S n@(Operator op))
>  = categorise n

> t_ (Name_S n@(Backticks b))
>  = categorise n

%---------------------------------------
Extensions

> t_ (Ext_S e@(Case es cs)) = fmap Ext $ mapM_Extension t_ e

> t_ (Ext_S e@(Rec _ _ _ _)) = fmap Ext $ mapM_Extension t_ e
> t_ (Ext_S e@(Sig _ _ _)) = fmap Ext $ mapM_Extension t_ e
> t_ (Ext_S e@(With _ _ _ _)) = fmap Ext $ mapM_Extension t_ e






%---------------------------------------
Catch-all case.

> t_ t
>  = error $ "new constr SimpleToTerm, t = " ++ show t


%-------------------------------------------------------------------------------
Handling of Binding Operators

> t_bind c n ty t
>  = do
>		ty' <- t_ ty
>		let ce = CE (Bind "?") ty'
>		t'  <- add_binder n ce $ t_ t
>		return (c n ty' t')

NB this DOESN'T DISTINGUISH FO from DP names! 
	Is this distinction useful?

%-------------------------------------------------------------------------------
This still ugly.

would like to streamline, avoid reference to context internals.


> categorise :: Name -> CERM Term
> categorise n
>  = flip try_another (categorise_univ n) $ 
>    do
>		(CE kind _) <- find n 
>		categorise_kind kind n

> categorise_univ :: Name -> String -> CERM Term
> categorise_univ n m
>  = useStateCERM (univLookup n) 
>		`try_another` \lm -> fail_with_msg (m ++ "\n(and: " ++ lm ++ ")")

> categorise_kind kind n
>  = case kind of 
>		(Def rhs atom)     -> return atom		-- should use field selector! 
>		(Hyp atom)         -> return atom		-- (but lose catch on error)
>		(M_V atom)         -> return atom
>		(ElimOp a atom)    -> return atom
>		(FnDef a f atom)   -> return atom
>		fam@IndFamily{}    -> return (atom fam)

>	--	(FnDef a f atom)   -> return $ Const n a f	-- unused.

>		(Bind _)           -> return $ Var n
>		(Let_ _)           -> return $ Var n
>		(Coercion{})       -> return $ atom kind 
>									-- treat like definition; should work (!)
>									-- could just return t, ie silently expand
>		_                  -> unknown
>    where
>		unknown = fail_with_msg $ "Unknown ce in categorise: " ++ show kind


%-------------------------------------------------------------------------------
Provisional! 


> translate_dp_arrow (Op_S l op@(Name_S (Operator "->")) r) e
>  = do
>		r' <- t_ r 
>		translate_dp_arrow l (DP Underscore r' e)

> translate_dp_arrow t e
>  = do
>		t' <- t_ t
>		return (DP Underscore t' e)
