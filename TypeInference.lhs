> module TypeInference (
>		inferType, 
>		inferKind,
>		typeCheck, 
>		typeCheckRawKind,
>		TypedTerm(..)		-- MOVE to Terms? 
>	) where

type synthesis and checking 

> import IOExts_(unsafePerformIO)

> import Base
> import Terms
> import SharedSyntax
> import TermOps(terminals)
> import TermReduction(substitute, first_IVar)
> import Context hiding (IM)
> import Convertibility(convertible, unifiable, Substitution(..))
> import Reduction(dwhnf)

> import Coercions(find_coercion, no_coercion) 

> import DebugOptions(traceOnDebugFlag)
> import GlobalOptions(canOmitEl)

> import Printing(showTerm, showTerm_, showTermAndType, showCtxt, CtxtShow(..))
> import Pretty(text, vcat, nest, (<+>), ($$), ($+$))
> import PrettyAux(longRender, shortRender)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Trace Functions.

use forced, switchable trace everywhere

> ts_trace    s r = traceOnDebugFlag "SYNTHESIS_TRACING"    (s `fseq` s) r
> no_ts_trace s r = r 

> ap_trace    s r = traceOnDebugFlag "SYNTHESIS_AP_TRACING" (s `fseq` s) r

> ts_tr s = traceOnDebugFlag "" (s `fseq` s) $ return ()

---
`it_trace'
  - detailed trace fn.

> it_trace :: Ctxt -> String -> Term -> OkF TypedTerm -> OkF TypedTerm
> it_trace c n ot r
>  = ts_trace (longRender msg) r
>    where
>		msg = vcat [
>				text $ "IT TRACE: (" ++ n ++ ")",
>				text "  a)" <+> showTerm c ot,
>				text "  b)" <+> elimOk (\m -> text $ "FAIL: " ++ m) show_tt r,
>				text ""]
>		show_tt tt@(TT t ty) 
>		 | same_term tt = text "<probably same>" $+$ show_ ty
>		 | otherwise    = showTerm c t           $+$ show_ ty
>		show_ ty = nest 2 (text ":" <+> showTerm c ty)

>		same_term (TT it _) = False		-- DISABLED, too slow.

<>		same_term (TT it _)
<>		 = synthesis_trace_if_set ("STARTING\n") $ 
<>		   (\r -> r `seq` synthesis_trace_if_set "FINISHED CMP\n" r) $
<>		   length (terminals it) == length (terminals ot)


---
`trace_Ap'
  - special case for Ap.
  - records the arguments before the conv test.
  - be nice to have a horizontal box join here....

> trace_Ap :: (Term,Term) -> (Term,Term) -> IM ()
> trace_Ap (t1,ty1) (t2,ty2) 
>  = do
>		c <- ctxt 
>		ap_trace ("BeforeConv Ap:\n" ++ longRender (ts c) ++ "\n") $ 
>			return ()
>    where
>		ts c = vcat [ showTerm c t1 $+$ nest 2 (text ":" <+> showTerm c ty1),
>		              showTerm c t2 $+$ nest 2 (text ":" <+> showTerm c ty2) ]




---
`trace_is_conv'
  - tracing of conversion test operations.
  - this just shows the arguments; it is only called when conv fails.

> trace_is_conv c it ot
>  = no_ts_trace (
>		"Synthesis Conv\n"
>		++ longRender (nest 2 $ 
>				text "IT:" <+> showTerm c it $$ text "OT:" <+> showTerm c ot)
>		++ showCtxt (Show_Some 5) c ++ "\n")



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

+ Prettyprinting in error messages? 

FIRST COERCION 
	- from Type to Kind.

%-------------------------------------------------------------------------------

> tt :: Functor m => Term -> m Term -> m TypedTerm		-- aux 
> tt t = fmap (TT t)


. try to detect a difference? 
. it probably doesn't matter... - cf unification...

%-------------------------------------------------------------------------------


> inferType :: Ctxt -> Term -> OkF TypedTerm
> inferType c t 
>  = change_msg ("inferType\n" ++) $
>    it_trace c "inferType" t      $ 
>    c_run c (it t)

%-------------------
`typeCheck'

> typeCheck :: Ctxt -> Term -> Type -> OkF TypedTerm
> typeCheck c t ty
>  = change_msg ("typeCheck\n" ++) $
>    it_trace c "typeCheck" t      $ 
>    c_run c                       $
>    do
>		tt@(TT t1 ty1) <- it t
>		ty1 `is_conv` ty
>		return tt

---
`typeCheckRawKind'
  - the type being checked against might not have necessary coercions inserted
  - so this function does so by inferring the type's type, and checking
    against that.

  - NB it returns the expected type (after it has been kind-checked)
	   this is to keep the more informative type, eg universes.

  - make sure inferred type is lifted to Kind, before comparison.

QQ: since everything should be kind, then this fn can disappear? 


> typeCheckRawKind :: Ctxt -> Term -> Type -> OkF TypedTerm
> typeCheckRawKind c t ty
>  = change_msg ("typeCheckRawKind\n" ++) $
>    it_trace c "typeCheckRawKind" t      $ 
>    do
>		(TT ty1 _) <- inferKind c ty 
>		(TT t1 _) <- do
>						(TT t' ty_t) <- typeCheck c t ty1
>						(TT ty_t' _) <- inferKind c ty_t
>						convertible c ty_t' ty1
>						return (TT t' ty_t')
>		return (TT t1 ty1)


---

> inferKind :: Ctxt -> Term -> OkF TypedTerm
> inferKind c t
>  = change_msg ("inferKind\n" ++) $
>    it_trace c "inferKind" t      $ 
>    c_run c (it t >>= check_is_kind "Kind")



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
`check_is_kind'
  - test for kindness, allowing for El-coercion.
  - Kind is obviously ok.

  - Any object of kind Type can be coerced to a Kind by applying El to 
    the object. This is only done if the `canOmitEl' flag is set.

  - use direct test for Type-ness, since no reduction can yield a kind.

  - everything else fails.

> check_is_kind :: String -> TypedTerm -> IM TypedTerm
> check_is_kind m tt@(TT t Kind)
>  = return tt
> check_is_kind m tt@(TT t _)		-- experimental, allows lets in kinds
>  | is_kind_Type t 
>  = return tt
> check_is_kind m (TT t ty)
>  = it_where "(kind-check)" $
>    do
>		-- ts_trace ("CIK = " ++ crude_print ty ++ "\n") $ return ()
>		if unsafePerformIO canOmitEl
>			then do
>					case ty of
>						Type -> return (TT (El t) Kind)			-- direct test
>						ContextVar (Plain "Prop")
>						     -> return (TT (Ap (ContextVar $ Plain "Prf") t) Kind)			-- direct test
>						_    -> fail_not_a_kind "type is not El-coercible"
>			else fail_not_a_kind "El-coercions not enabled"
>    where
>		fail_not_a_kind msg = it_err (m ++ " not a kind: " ++ msg ++ ".\n") t ty

---
`check_isn't_kind'
  - related to above, but the reverse.
  - NB since Kind isn't a kind, we don't need reduction tests etc.

> check_isn't_kind :: String -> TypedTerm -> IM TypedTerm
> check_isn't_kind m (TT t Kind) 
>  = it_err (m ++ " should not be a kind.\n") t Kind
> check_isn't_kind m tt          
>  = return tt


%-------------------------------------------------------------------------------
MOVE ? 
REDO with distinction between TypedTerm and Kinds? 
 (lcf - could get H~ type system to support?)

> kind :: Monad a => a Type
> kind = return Kind

> has_kind_Type :: Monad a => Type -> a TypedTerm
> has_kind_Type t = return (TT t Type)

---
`is_conv' compares the inferred type to the original type.

(NB this isn't very robust)

> type Inf_Type = Type
> type Orig_Type = Type

> is_conv :: Inf_Type -> Orig_Type -> IM ()
> is_conv Kind Kind
>  = return () 

> is_conv it ot
>  = change_msg ("Conversion test\n" ++) $ 
>    do
>		c <- ctxt
>		case (convertible c it ot) of
>			Ok ()  -> return ()
>			Fail m -> trace_is_conv c it ot $ fail_with_msg m




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Monad for type inference

> type IM a = CERM a

> lookup_type :: Name -> IM Type
> lookup_type i
>  = do
>	c <- ctxt
>	case univLookup i c of
>	  Ok (Ext (HardCast _ ty)) -> return ty
>	  Fail _ -> do 
>			(CE _ ty) <- find i
>			return ty

> lookup_db_type :: DB_Index -> IM Type		-- special case for indices.
> lookup_db_type i 
>  = do 
>		c <- ctxt 
>		return (snd $ findCtxt_DB i c)

NB vars should all be known, so these calls should not return Undef.

> lookup_let_info :: DB_Index -> IM (Term,Type)
> lookup_let_info i
>  = do 
>		c <- ctxt
>		return $ findCtxt_DB_let i c



---
`it_err' - show function for errors.

> it_err :: String -> Term -> Type -> IM a
> it_err m t ty
>  = ctxt >>= \c -> 
>    fail_with_msg $ 
>			m ++ "\n" 
>			++ showTermAndType c t ty ++ "\n"
>			++ showCtxt (Show_Some 5) c ++ "\n" 

<>			-- "    == " ++ take 60 (crude_print t) ++ "\n" ++
<>			-- "    :: " ++ take 60 (crude_print ty) ++ "\n" ++

---
`it_where' 
  - record where we've been, for error reporting. 

  - CRUDE - should embellish the Fail type. 

> it_where :: String -> IM a -> IM a
> it_where s
>  = ts_trace (s ++ " type synthesis\n")

<>  = change_msg ((s ++ " type synthesis\n") ++)



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Actual type inference.

defined by str. recursion, according to Luo's algorithm.

> it :: Term -> IM TypedTerm

> it Kind              = fail_with_msg "Kind has no type."

> ------ added
> it t@(ContextVar (Plain "Prop"))  = (tt t) $ kind
> it t@(Ap (ContextVar (Plain "Prf")) p)
>  = do
>	(TT p1 p_ty) <- it p 
>	assert_p (is_kind_Prop p_ty)
>		 "Prf can only be applied to Props" 
>	tt (Ap (ContextVar (Plain "Prf")) p1) $ kind
> ------


> it t@(Type)          = (tt t) $ kind

> it t@(Elim i _)      = (tt t) $ lookup_type i
> it t@(F_Elim i)      = (tt t) $ lookup_type i		-- is variant of (Elim i)
> it t@(Const i _)     = (tt t) $ lookup_type i
> it t@(F_Const i _)   = (tt t) $ lookup_type i

> it t@(Closed o _)    = it o  -- revert to original.

> it t@(IVar i)        = (tt t) $ lookup_db_type i

> it t@(LocalDef i _)  = do
>							(t,ty) <- lookup_let_info i
>							return $ TT (LocalDef i t) ty

> it t@(GlobalDef i _) = (tt t) $ lookup_type i
> it t@(ContextVar v)  = (tt t) $ lookup_type v
> it t@(MetaVar v)     = (tt t) $ lookup_type v

> -- it t@(Var v)         = (tt t) $ lookup_type v 
> it t@(Var v)         = fail_with_msg $ "Var (" ++ show v ++ ") in 'it'"


Recursive Cases

> it (El t)
>  = it_where "El" $
>    do 
>		(TT t1 ty_t) <- it t
>		if is_kind_Type ty_t
>			then tt (El t1) $ kind
>			else do
>					c <- ctxt 
>					fail_with_msg $ "El requires a type\n" ++ showTerm_ c t


> it (DP v ty t)
>  = it_where "DP" $
>    do
>		(TT ty1 ty_ty) <- it ty >>= check_is_kind "DP type" 

>		(TT t1  ty_t) <- change (\c -> addCtxt_BV v ty1 c) $
>		                 it t >>= check_is_kind "DP body"
>		(tt $ DP v ty1 t1) $ kind

> it (FO v ty t)
>  = it_where "FO" $
>    do
>		(TT ty1 ty_ty) <- it ty >>= check_is_kind "FO type"
>		(TT t1 ty_t) <- change (\c -> addCtxt_BV v ty1 c) $
>		                it t >>= check_isn't_kind "FO body"
>		return  (TT (FO v ty1 t1) (DP v ty1 ty_t))

<> ts_trace ("FO post-check: " ++ crude_print t1 ++ "\n") (return ())

---

----
Let:
 - Let defs are handled by placing (shifted) definiens on each occurrence,
   so we mainly recurse on the `bound' term.
 - no context change is made, since the necessary information is already in
   the subterm (t2)
 - for completeness, we synthesise the type (nb plus coercion-insert) the 
   definiens, so it is shown correctly.
 - this means possible repetition, but this is necessary unless Let vars are
   given deBruijn indices.

> it t@(Let v t1 t2) 
>  = do
>		-- ts_trace ("Let " ++ show (v,t1,t2) ++ "\n") $ return ()
>		(TT t1' ty_t1) <- it t1
>		-- ts_trace ("Let2 " ++ show (t1',ty_t1) ++ "\n") $ return ()
>		(TT t2' ty_t2) <- change (addCtxt_Let v t1' ty_t1) $ it t2
>		return $ TT (Let v t1' t2') (Let v t1' ty_t2)


---

> it (Ap lt rt)
>  = it_where "Ap" $
>    do
>		(TT lt1 ty_lt) <- it lt
>		(a,b) <- case (dwhnf ty_lt) of	-- but reduction not reqd for kinds? 
>					DP x a b -> return (a,b)
>					not_FO   -> it_ap_err "Ap left type not DP:" lt ty_lt

>		rtt@(TT rt1 ty_rt) <- it rt
>		trace_Ap (lt,ty_lt) (rt1,ty_rt)

>		c_term <- conv_with_coercion rtt a
>		return (TT (Ap lt1 c_term) (substitute b c_term))


COERCION preservation also an issue here.		EH? (apr 99)

---

This case is necessary if we are checking results from another algorithm,
eg from ConstraintTypeInf/metavars.

IMPORTANT:
 - it strictly checks type-validity of the coercion application 
 - ie, we may NOT add more coercions! 

 - NB in theory, we should check the term and return it unchanged
 - but I allow for possibility of change (eg El insertion).

> it (Co f t)
>  = do
>		(TT f' ty_f) <- it f
>		(a,b) <- case (dwhnf ty_f) of
>					DP x a b -> return (a,b)
>					not_FO   -> it_ap_err "Co left type not DP:" f ty_f
>		(TT t' ty_t) <- it t
>		ty_t `is_conv` a
>		return (TT (Co f' t') (substitute b t'))

---

> it (Ext (SoftCast e t))
>  = do
>		(TT e' ty_e) <- it e
>		(TT t' ty_t) <- it t
>		ty_e `is_conv` t'
>		return (TT (Ext (SoftCast e' t')) t')

Can't infer type of e, so can't insert coercions...
But infer type of t just for coercions etc.
QQ: need to check if result is a kind? 

---
CAST handling.
 - CAST means use the supplied type without checking the cast term
 - but we still have to check the validity of the given type.
 - (NB should be a kind check?)
 - When nested CASTS: cast means override type, so we ignore the outer type

> it (Ext (HardCast e@(Ext (HardCast _ _)) _))
>  = it e

> it (Ext (HardCast e t))
>  = do
>		(TT t' ty_t) <- it t
>		return (TT (Ext (HardCast e t')) t')

%-------------------------------------------------------------------------------
RECORDS.

---
opaque record type
		L : Type        (* L may be a record type, and may have a field r *)
		A : (L)Type
		--------------
		<L,r:A> : Type

> it (Ext (Sig l r (Opaque a)))
>  = do 
>		(l',a') <- it_check_wf_opaque_sig l a
>		has_kind_Type (Ext (Sig l' r (Opaque a'))) 

---
manifest record type
        <L,r:A> : Type
        a : (l:L)->A(l)
        ----------------
        <L,r=a:A> : Type

> it (Ext (Sig l r (Manifest v ty)))
>  = it_where "Sig manifest" $
>    do
>		(l', ty') <- it_check_wf_opaque_sig l ty

>		(TT v' ty_v) <- it v

>		ts_trace ("MR: " ++ show ty_v ++ " , " ++ show ty' ++ "\n") $ return ()

>		let expected_ty = DP (Bound $ Plain "l") (El l') (El $ Ap ty' first_IVar)
>		ts_trace ("ET: " ++ show expected_ty ++ "\n") $ return ()

>		ty_v `is_conv` expected_ty

>		has_kind_Type (Ext (Sig l' r (Manifest v' ty'))) 

---
intro, opaque record

        <L,r:A> : Type
        l : L
        a : A(l)
        -----------------
        <l,r=a> : <L,r:A>


> it (Ext (Rec l r a r_ty))
>  = it_where "Str" $
>    do
>		(TT l' l_kind) <- it l 
>		l_ty <- case l_kind of
>					El t  -> return t
>					other -> fail_with_msg $ 
>								"Kind of Record base isn't a Type" ++ show other
>			-- check whether l has El-kind, in which case its type is a Type
>			-- is there a better way to do this? 

>		(l_ty', r_ty') <- it_check_wf_opaque_sig l_ty r_ty
>		-- assert: l_ty' = l_ty (since returned by 'it' before)
>		-- maybe worse - REPEATED WORK! (But, kind=Type check needed!)
>		-- fn strips off the El - hacky! 

>		(TT a' a_ty) <- it a
>		-- ts_tr $ "rec: " ++ show (a',a_ty) ++ "\n"

>		a_ty `is_conv` (El (Ap r_ty' l))
>		-- a_ty will be a kind. 

>		return $ TT (     Ext (Rec l'    r a' r_ty')) 
>					(El $ Ext (Sig l_ty' r    (Opaque r_ty')))

---

        <L,r:A> : Type
        -----------------------
        <L,r:A>.r = A : (L)Type

        l : <L,r:A> 
        -----------      
        l.r : A(l|r)  

        l : <L,r:A>
        l|r.p : P
        ----------- (r =/= p)
        l.p : P

checking the sig WF ok here? not sure.


> it (Ext (Project l p __TODO_XXXX _))
>  = it_where "Rec Project" $ 
>    do
>		(TT l' ty_l) <- it l
>		(TT ty_l_el _) <- it ty_l >>= check_is_kind "Project" 
>		ty_l' <- case ty_l_el of
>					El t -> return t
>					Type -> do	-- static projection.
>								fail_with_msg "No static projection yet\n"
>					other -> fail_with_msg $ "Proj, other kind = " ++ show other

>		case (dwhnf ty_l') of
>			Ext (Sig m r (Opaque a))
>			 -> if r == p
>				then return (TT (proj l' p)
>				                (El $ Ap a (bar l' p)))
>				else do
>						let lrp = proj (bar l' r) p
>						(TT lrp' lrp_ty) <- it lrp
>						ts_tr ("DOT-opaque, get = " ++ show (get lrp') ++ "\n")
>						return (TT (proj_m l' p (get lrp')) lrp_ty)

>			Ext (Sig m r (Manifest a a_ty))
>			 -> if r == p
>				then return (TT (proj_m l' p (Just a))
>                               (El $ Ap a_ty (bar l' p)))
>				else do
>						let lrp = proj (bar l' r) p		-- under a manifest
>						(TT lrp' lrp_ty) <- it lrp
>						ts_tr ("DOT-manifest, get = " ++ show (get lrp') ++ "\n")
>						return (TT (proj_m l' p (get lrp')) lrp_ty)

>			other -> error $ "Project, type, other = " ++ show other
>    where
>		-- proj_manifest l = Ext . Project (Ext (SoftCast l 
>		bar  l n     = Ext $ Restrict l n []
>		proj l n     = Ext $ Project l n [] Nothing
>		proj_m l n a = Ext $ Project l n [] a

>		get (Ext (Project _ _ _ maybe)) = maybe
>		get _                           = error "get not possible" -- Nothing




---
almost identical to above...


        <L,r:A> : Type
        -----------------------
        <L,r:A>|r = L : Type

        l : <L,r:A> 
        -----------     
        l|r : L

        l : <L,r:A>
        l|r|p : P
        ----------- (r =/= p)
        l|p : P

> it (Ext (Restrict l p __TODO_XXXX))
>  = it_where "Rec Restrict" $ 
>    do
>		(TT l' ty_l) <- it l
>		(TT ty_l_el _) <- it ty_l >>= check_is_kind "Restrict" 
>		ty_l' <- case ty_l_el of
>					El t -> return t
>					Type -> do	-- static restriction.
>								fail_with_msg "No static restriction yet\n"
>					other -> fail_with_msg $ "Restr, other kind = " ++ show other
>		case (dwhnf ty_l') of
>			Ext (Sig m r _)		-- doesn't matter whether manifest or opaque
>			 -> if r == p
>				then return (TT (bar l' p) (El m))
>				else do
>						let lrp = bar (bar l' r) p
>						(TT lrp' lrp_ty) <- it lrp
>						return (TT (bar l' p) lrp_ty)
>			other -> error $ "Restrict, l-type other = " ++ show other 
>    where
>		proj l n = Ext $ Project l n [] Nothing
>		bar  l n = Ext $ Restrict l n []

---

> it (Ext (With l n v _)) 
>  = it_where "Rec With" $
>    do
>		error "no with it-rule yet"


---

> it t = unknown_constructor "inferType" t

---

> it_ap_err :: String -> Term -> Type -> IM a
> it_ap_err m t ty
>  = ctxt >>= \c -> 
>    fail_with_msg $ 
>			m ++ "\n" ++ 
>			"    == " ++ shortRender (showTerm c t)  ++ "\n" ++
>			"    :: " ++ shortRender (showTerm c ty) ++ "\n" 

<>			++ 
<>			"    :: " ++ shortRender (showTerm c ty) ++ "\n" ++
<>			showInCtxt c t ++ "\n" 


%-------------------------------------------------------------------------------
Common function for record signatures.

opaque record type
		L : Type        (* L may be a record type, and may have a field r *)
		A : (L)Type
		--------------
		<L,r:A> : Type

IMPORTANT:
	l must have kind Type.

> it_check_wf_opaque_sig :: Type -> Type -> IM (Type,Type) 
>		-- takes values as {L r A} 
>		-- maybe should use more informative type - esp in return. 
>		-- although TT is often useful

> it_check_wf_opaque_sig l a
>  = it_where "checking WF of opaque signature" $
>    do
>		-- ts_tr ("ICWF in: " ++ show (l,a) ++ "\n")
>		(TT l' ty_l) <- it l
>		-- ts_tr ("ICWF l_ty: " ++ show ty_l ++ "\n")
>		assert_p (is_kind_Type ty_l) ("Sig record not Type: " ++ show ty_l)
>		(TT a' ty_a) <- it a
>		bd_ty <- case ty_a of			-- note the direct test to Type
>					DP _ ty Type -> return ty
>					t            -> fail_with_msg $
>										"Field type - not valid DP: " ++ show t
>		El l' `is_conv` bd_ty 
>		return (l', a')
>			-- might prefer (El l') ?
>			-- need to think about this.

<> it_check_wf_opaque_sig l a 
<>  = fail_with_msg $ "ITWF err, " ++ show (l,a)

 - Where should the El go? 
 - tmp - on the l' in conv test.
 - ditto for below

%-------------------------------------------------------------------------------

rt = required type
it = inferred type

> conv_with_coercion tt@(TT t it) rt
>  = -- change_msg (const $ "Not convertible.") $
>    try_coercion_if_fail tt rt $
>    do
>		it `is_conv` rt 
>		return t

> try_coercion_if_fail tt rt m 
>  = m `try_another` \s -> 
>    (do
>		c <- ctxt
>		(_, c_term) <- embed $ find_coercion c tt rt s

>		-- check validity!
>		-- have to do steps explicitly (else failures causes inf loop...)
>		(TT _ ty_c_term) <- it_where "Coercion validity" $ it c_term

>		c <- ctxt
>		assert_p (isOk $ convertible c ty_c_term rt) "Bad coercion returned:" 

>		return c_term )
>    `try_another` \s2 ->
>	 fail_with_msg s		-- original msg



