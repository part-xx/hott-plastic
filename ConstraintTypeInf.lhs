> module ConstraintTypeInf (
>                inferType
>                , findCoercion
>                , CoInfo(..)
>                , WhichCoercion(..)        -- move elsewhere? 
>                ) where

Combined Type Synthesis/ Checking/ Metavar calcs.

> import IOExts_(unsafePerformIO)
> import Control.Monad(foldM)
> import Data.List(intersect, partition)
> import Data.Maybe(catMaybes, fromMaybe)

> import BaseClasses(Fallible(..), Embeddable(..)) 
> import BaseTypes(OkF(..), elimOk, isFail, isOk, fromFail)
> import BaseFunctions(splitEither, pair)
> import Base(Forceable(..), assert_p)

> import Debugging(show_term, hack_show)

> import DebugOptions(traceOnDebugFlag)
> import GlobalOptions(canOmitEl)

> import Terms
> import SharedSyntax(CoercionName(..))
> import TermOps(namesIn, subtrees, replaceTerminal, terminals)
> import TermOps(addDeBruijnIndices, un_DP)
> import TermReduction(substitute, db_start, shiftOverOneBinder, first_IVar)
> import TermReduction(shiftUnderSomeBinders, shiftUnderOneBinder)
> import Context hiding (IM)

> import Printing(showTerm, showTerm_, showTermAndType, showCtxt, CtxtShow(..))

> import Pretty(text, vcat, colon, (<+>))
> import PrettyAux(longRender, stext)

> import Reduction(FlatTerm(..), dwhnf, flatten, unflatten)
> import Reduction(etaContract, reducibleApplication)
> import Convertibility(unifiable, Substitution(..), showSubstitution)
> import Convertibility(applySubstitution,emptySubstitution,isEmptySubstitution)

#ifndef __HASKELL98__
#define fmap map
#endif

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Tracing etc

> it_where :: String -> a -> a
> it_where s = tr ("C_IT case = " ++ s ++ "\n")

> -- tr s = traceOnDebugFlag "" (s `fseq` s)
> tr s = traceOnDebugFlag "METAVAR_TRACING" (s `fseq` s ++ "\n")
> mtr s = tr s $ return ()

> -- ctr s = traceOnDebugFlag "" (s `fseq` s)
> ctr s = traceOnDebugFlag "COERCION_TRACING" (s `fseq` s ++ "\n")
> mctr s = ctr s $ return ()


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


> newtype ST_OK r s a = ST_OK (r -> s -> OkF (s,a))        -- fail discards state

<> {-# SPECIALISE instance Functor ((->) String) #-}
<> {-# SPECIALISE instance Functor ((->) a) #-}

> instance Functor (ST_OK r s) where
>    fmap f (ST_OK m) = ST_OK $ \r -> \s -> fmap (\(s,a) -> (s,f a)) (m r s)

> instance Monad (ST_OK r s) where
>    return a = ST_OK $ \r -> \s -> Ok (s,a)
>    (ST_OK m) >>= f 
>     = ST_OK $ \r -> \s -> 
>                do
>                    (s',a) <- m r s 
>                    let (ST_OK m2) = f a 
>                    m2 r s'
#ifdef __HASKELL98__
>    fail = fail_with_msg
#else 
<> instance MonadZero (ST_OK r s) where
<>    zero = fail_with_msg "monadzero fail, eg pattern match"
#endif

---

> run_ST :: ST_OK r s a -> r -> s -> OkF (s,a)
> run_ST (ST_OK f) = f

> chg :: (s -> s) -> ST_OK r s ()
> chg f = ST_OK $ \r -> \s -> Ok (f s, ())

> get :: ST_OK r s s 
> get = ST_OK $ \r -> \s -> Ok (s,s)


> lift :: ST_OK r s a -> ST_OK r s (OkF a)
> lift (ST_OK m) 
>  = ST_OK $ \r -> \s -> 
>                Ok $ elimOk (\m -> (s, Fail m)) (\(s1,a) -> (s1,Ok a)) (m r s) 

> okfToList (Fail _) = []
> okfToList (Ok ss)  = [ss]

---
splice: 
 - lift M, allow it to continue at the point it came from. 
 - ie, can do m-like things but don't lose local state (works?)

> splice :: ST_OK r s a -> ST_OK r s (ST_OK r s a)
> splice (ST_OK f) = ST_OK $ \r -> \s -> Ok (s, ST_OK $ \_ -> \_ -> f r s)

<> splice (ST_OK f) = ST_OK $ \r -> \s -> Ok (s, ST_OK $ \_ -> \_ -> f r s)


<> restart :: (r,s,a) -> ST_OK r s a
<> restart (r,s,a) = ST_OK $ \r s ->  eh? 



---
`subst' 
  - performs the current substitution on the given term. 

> subst :: Term -> M Term
> subst t 
>  = do
>        s <- get
>        let ps = plain_substitution s 
>        return $ applySubstitution ps t

> subst_tt :: TypedTerm -> M TypedTerm
> subst_tt (TT t ty) 
>  = do
>        s <- get
>        let ps = plain_substitution s 
>        return $ TT (applySubstitution ps t) (applySubstitution ps ty)

---

> instance Fallible (ST_OK r s) where
>    fail_with_msg m = ST_OK $ \r -> \s -> Fail m
>    change_msg sf (ST_OK m) = ST_OK $ \r -> \s -> elimOk (Fail . sf) Ok (m r s)

>    try_another m1 m2
>     = lift m1 >>= elimOk m2 return

<> instance Embeddable (ST_OK s) where


%-------------------------------------------------------------------------------
Types and Monad(-ish) types.

---
More descriptive than plain maybe.

HOW ABOUT: representation for "anything-but-kind" ? 

> data TypeConstraint
>  = No_Constraint
>  | Constraint Type
>  | Coercible Type
>  deriving Show

> unTC :: TypeConstraint -> Type
> unTC No_Constraint   = error "unTC No_Constraint"
> unTC (Constraint ty) = ty
> unTC (Coercible ty)  = ty

---
---

> type M a = ST_OK FreeNameSupply MV_Ctxt a
> type MM a = TypeConstraint -> M a

> type IM = MM TypedTerm

---
`get_free_names_generator'
  - access to the const state value.

> get_free_names_generator :: M FreeNameSupply
> get_free_names_generator = ST_OK $ \r -> \s -> return (s,r)

%-------------------------------------------------------------------------------

> attach :: Term -> M Type -> M TypedTerm
> attach t = fmap $ TT t 

---

> lookup_type_db :: Ctxt -> DB_Index -> M Type
> lookup_type_db c i
>  = return (snd $ findCtxt_DB i c)

> lookup_type :: Ctxt -> Name -> M Type
> lookup_type c n 
>  = embed (findCtxt n c >>= \(CE _ ty) -> return ty)

---
`ty_ch'
  - lift (M x) by "using" the MTY, by type checking against it

> ty_ch :: Ctxt -> M TypedTerm -> MM TypedTerm
> ty_ch c m No_Constraint
>  = m :: M TypedTerm

> ty_ch c m (Constraint Kind)
>  = it_where "Kind checking" $
>    do
>        tt@(TT t ty) <- m
>        let fail_here msg = it_err c (msg ++ ".\n") t ty
>        case ty of 
>            Kind -> return tt
>            Type -> do
>                        if unsafePerformIO canOmitEl
>                          then return (TT (El t) Kind)
>                          else fail_here "El-coercions not enabled"
>            ContextVar (Plain "Prop") -> do
>                        if unsafePerformIO canOmitEl
>                          then return (TT (Ap (ContextVar $ Plain "Prf") t) Kind)
>                          else fail_here "El-coercions not enabled"
>            _    -> fail_here "resulting type is not El-coercible"

> ty_ch c m cstr@(Constraint ety)
>  = ty_ch c m (Coercible ety)        -- EXPERIMENTAL.

---
EXPERIMENTAL
MIGHT WANT TO MAKE ANY CONSTRAINT MEAN A COERCION.
MAYBE WANT TO UNIFY CODE WITH ABOVE ANYWAY? 

> ty_ch c m cstr@(Coercible ety)
>  = it_where "COERCIBLE - non-Kind type checking" $
>    do
>        tt@(TT t ty) <- m 
>        mtr ("TC t/ty = " ++ show ty ++ ", " ++ show ety)
>        ss <- lift $ unify_with_mvs c ty ety 
>        mtr ("RAW SS = " ++ show ss)
>        case ss of 
>            Ok ss  -> use_ss ss (TT t ety)
>            Fail m -> prepend_msg m $
>                            fmap snd (find_coercion c (coercions c) tt ety)
>                            `try_another_` 
>                            try_with_new_mvar c tt cstr

---
OLD Constraint case, used when EXACT match is reqd
(else coercion search can loop...)

> ty_ch_exact c m cstr@(Constraint ety)
>  = it_where "non-Kind type checking" $
>    do
>        tt@(TT t ty) <- m 
>        mtr ("TC t/ty = " ++ show ty ++ ", " ++ show ety)
>        ss <- lift $ unify_with_mvs c ty ety 
>        mtr ("RAW SS = " ++ show ss)
>        case ss of 
>            Ok ss  -> use_ss ss (TT t ety)
>            Fail m -> try_with_new_mvar c tt cstr
>                        `try_another_` fail_with_msg m


> ty_ch_exact c m cstr
>  = error $ "ty_ch_exact, illegal constraint: " ++ show cstr


%-------------------
`try_with_new_mvar'
  - See if adding a metavar to a term with DP kind gets anywhere.

Oct-00: 
    XXX this should change.
    only allow new mvars at top? 

> try_with_new_mvar :: Ctxt -> TypedTerm -> TypeConstraint -> M TypedTerm
> try_with_new_mvar c tt_fn@(TT t (DP _ ty body)) constraint
>  = it_where ("try new mv: " ++ show ty ++ ", " ++ show constraint) $ 
>    do
>        tt_mv@(TT _ _) <- handle_new_var c "?" (Constraint ty)
>        new_tt <- apply_to_TT Ap c (return tt_fn) tt_mv
>        ty_ch c (return new_tt) constraint

*** NOTE - can use apply_to_TT here! also chg sig. to TT. 

> try_with_new_mvar c (TT t _) _
>  = fail_with_msg "Can't add metavar - not DP"


%-------------------------------------------------------------------------------
`apply_to_TT'
 - avoid re-infer type of argument, and unify its type with DP of term.
 - so this is a cut-down version of the Ap case. 
 - return full result of ap. 

 - want result out to be what? list of unsolved metavars? 

> apply_to_TT 
>  :: (Term -> Term -> Term) -> Ctxt -> M TypedTerm -> TypedTerm -> M TypedTerm
> apply_to_TT ap_or_co c m_t (TT arg arg_ty)
>  = prepend_msg "apply_to_TT, " $
>    do
>        (TT t' ty_t) <- m_t 
>        -- traceOnDebugFlag "" ("Done itc " ++ "\n") $ return ()
>        (dom, rng) <- case ty_t of
>                        DP v ty t -> return (ty,t)
>                        _         -> fail_with_msg "applied term not a DP"

>        -- traceOnDebugFlag "" ("(dom,aty) = " ++ show (dom,arg_ty) ++ "\n") $ return ()
>        ss <- unify_with_mvs c dom arg_ty
>        mctr "Domain and arg type are unifiable in apply_to_TT"
>        let ap_term = ap_or_co t' arg
>        let ap_type = substitute rng arg
>        use_ss ss (TT ap_term ap_type)

----

> apply_to_TT_C
>  :: (Term -> Term -> Term) -> Ctxt -> M TypedTerm -> TypedTerm -> MM TypedTerm
> apply_to_TT_C ap_or_co c m_t (TT arg arg_ty) (Constraint ety)
>  = prepend_msg "apply_to_TT, " $
>    do
>        (TT t' ty_t) <- m_t 
>        -- traceOnDebugFlag "" ("Done itc " ++ "\n") $ return ()
>        (dom, rng) <- case ty_t of
>                        DP v ty t -> return (ty,t)
>                        _         -> fail_with_msg "applied term not a DP"

>        mctr $ "(dom,aty) = " ++ show (dom,arg_ty)

>        ok_ss <- lift $ unify_with_mvs c dom arg_ty
>        case ok_ss of
>            Ok ss  -> do
>                        mctr "Domain & arg type unify in apply_to_TT_C"
>                        let ap_term = ap_or_co t' arg
>                        let ap_type = substitute rng arg
>                        use_ss ss (TT ap_term ap_type)
>            Fail m -> do
>                        mctr "Domain & arg type DID NOT UNIFY in apply_to_TT_C"
>                        -- try doing result first.
>                        -- heuristic, since arg might need coercion? 
>                        let ap_type = substitute rng arg
>                        ok_ss <- lift $ unify_with_mvs c ap_type ety
>                        case ok_ss of
>                            Fail _ -> do 
>                                        mctr "Ranges NOT unifiable"
>                                        fail_with_msg m 
>                            Ok ss  -> do 
>                                        mctr "Ranges did unify"
>                                        fail_with_msg "CASE NYI"




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

> it :: Ctxt -> Term -> IM

%-------------------
EASY CASES

> it c Kind 
>  = \mty -> fail_with_msg "Kind has no type."
> it c t@(Type)          = ty_ch c $ return (TT t Kind)

> ------ added
> it c t@(ContextVar (Plain "Prop"))  = ty_ch c $ return (TT t Kind)

> it c t@(Ap (ContextVar (Plain "Prf")) p)
>  = ty_ch c
>  $ do
>       (TT p1 p_ty) <- it c p No_Constraint
>--       (Constraint $ ContextVar (Plain "Prop"))
>       assert_p (is_kind_Prop p_ty)
>                "Prf can only be applied to Props"
>       return $ TT (Ap (ContextVar (Plain "Prf")) p1) Kind
> ------


> it c t@(IVar i)        = ty_ch c $ attach t $ lookup_type_db c i
> it c t@(LocalDef i _)  = ty_ch c $ attach t $ lookup_type_db c i

> it c t@(Elim i _)      = ty_ch c $ attach t $ lookup_type c i
> it c t@(F_Elim i)      = ty_ch c $ attach t $ lookup_type c i
> it c t@(Const i _)     = ty_ch c $ attach t $ lookup_type c i
> it c t@(F_Const i _)   = ty_ch c $ attach t $ lookup_type c i

> it c t@(Closed o _)  = it c o	
>   -- NB drops compilation
>   -- but, user shouldn't be entering such terms anyway

> it c t@(GlobalDef i _) = ty_ch c $ attach t $ lookup_type c i
> it c t@(ContextVar v)  = ty_ch c $ attach t $ lookup_type c v


%-------------------
INTERESTING CASES

%-------------------
Var and MetaVar

> it c (MetaVar n)             = handle_metavar c n
> it c (Var (Plain n@('?':_))) = handle_new_var c n

> it c (Var v)                 = \mty -> fail_with_msg $ 
>                                        "Var (" ++ show v ++ ") in 'it'"


%-------------------
El

> it c (El t)
>  = \mty ->
>    it_where "El" $
>    do
>        assert_p (is_Kind_or_Nothing mty) 
>                 ("El constraint invalid: " ++ showTerm_ c (unTC mty))
>        (TT t1 ty_t) <- it c t (Constraint Type)
>        case under_lets ty_t of
>            Type -> return (TT (El t1) Kind)
>            _    -> fail_with_msg $ "El requires a Type\n" ++ showTerm_ c t
>    where
>        under_lets (Let _ _ t) = under_lets t
>        under_lets t           = t                -- candidate for mv. 


%-------------------
CASTS and COERCIONS

> it c (Ext (SoftCast e t))
>  = \mty -> 
>    it_where "Cast" $
>    do
>        error "Cast case not yet finalised"
>        (TT t' ty_t) <- it c t (Constraint Kind) 
>        tt@(TT e' ty_e) <- it c e (Constraint t') 
>        (TT e2 t2) <- ty_ch c (return tt) (Constraint t')
>        ty_ch c (return $ TT e' t') mty         -- RECAST? 

UNSURE ABOUT RESULT ABOVE - need ensure t' is retained.

> it c (Ext (HardCast e t))
>  = \mty -> 
>    it_where "CAST" $
>    do
>        (TT t' ty_t) <- it c t (Constraint Kind) 
>        -- mtr $ "CAST = " ++ show (t', mty)
>        ty_ch c (return $ TT (Ext (HardCast e t')) t') mty



%-------------------
BINDERS

NOTE: be careful about passing MTY down under binders. 
      ideally, should type check at the top? 

> it c (DP v ty t)
>  = \mty -> 
>    it_where "DP" $
>    do
>        assert_p (is_Kind_or_Nothing mty) 
>                 ("DP constraint invalid: " ++ showTerm_ c (unTC mty))
>        (TT ty1 ty_ty) <- error_place "DP type" $ 
>                          it c ty (Constraint Kind)
>        (TT t1  ty_t)  <- error_place "DP body" $ 
>                          do
>                            enter_binder (addCtxt_BV v ty1 ) c t (Constraint Kind)
>        subst_tt (TT (DP v ty1 t1) Kind)
>    where
>        error_place p = change_msg (("In " ++ p ++ ", ")++)

%---------------------------------------
Notes:
 - domain type is used as a constraint on value, not on type, hence
   the unification test AFTER type-inference.
 - then, range type is passed as constraint to enter_binder
 
> it c (FO v ty t)        -- :: DP v ty T 
>  = \mty ->
>    it_where "FO" $
>    do
>        let dom_constraint No_Constraint             = Nothing
>            dom_constraint (Constraint (DP _ dom _)) = Just dom
>            dom_constraint (Coercible  (DP _ dom _)) = Just dom
>            dom_constraint _                         = Nothing
>             -- NEW: throws info away, since not reliable
>             -- PREFER: pass whole info then we make best match locally
>             -- EXAMPLE: Refine Ind {?} [x:?]t ..., where ? omitted

>        tt@(TT ty1 _) <- it_where ("In FO type") 
>                      $ it c ty (Constraint Kind)

>        (TT ty1' _) <- case (dom_constraint mty) of
>                         Nothing  -> return tt
>                         Just dom -> do 
>                                      ss <- unify_with_mvs c dom ty1
>                                      use_ss ss tt
>                                         -- unify domains, if possible.

>        let range_constraint = case mty of
>              No_Constraint             -> No_Constraint
>              Constraint (DP _ _ rng) -> Constraint rng
>              Coercible  (DP _ _ rng) -> Coercible  rng
>              _                         -> error "bad R constraint in CTC/FO."

>        (TT t1 ty_t)   
>            <- do
>                    tt@(TT t ty)
>                        <- enter_binder (addCtxt_BV v ty1') c t range_constraint
>                    case ty of
>                        Kind -> fail_with_msg "FO Body can't be Kind"
>                        _    -> return tt
>        subst_tt (TT (FO v ty1' t1) (DP v ty1' ty_t))


> it c (Let v e t)                -- propagate mty, but shift it first
>  = \mty -> 
>    it_where "Let" $
>    do
>        (TT e' ty_e) <- it_where ("Let definiens") $ it c e No_Constraint
>        -- mtr $ "LET DEF ty = " ++ show ty_e
>        let shifted_mty = shift_down mty
>         -- need shift, since this is a faked binder.
>        (TT t' ty_t) <- enter_binder (addCtxt_Let v e' ty_e) c t shifted_mty
>        subst_tt (TT (Let v e' t') (Let v e' ty_t))

%-------------------
AP 

[1] - expensive cheat - better to call common factored code for right-ap case.
    - NB here is does NOT matter if no downward constraint.

[2] - need BOTH that R has a type and has downward constraint.


---
TRY - use r/mty to fix left, then continue as before.

> it c (Ap l r)
>  = \mty -> 
>    it_where ("Ap " ++ show (l,r)) $ 
>    do
>        -- show_mv_ss "SS In Ap"

>        (TT l' l_ty) <- it c l No_Constraint 
>                                       `try_another` it_left_mvars c l r mty
>        let d_l_ty = dwhnf l_ty
>        (domain, left_dp_type) <- case d_l_ty of
>                                    DP v d r -> return (d, d_l_ty)
>                                    not_FO   -> fail_with_msg "Ap left isn't FO"
>        -- mtr ("Dom = " ++ show domain)

>        (TT r' r_ty) <- do
>                            r_ <- subst r 
>                            it c r_ (Coercible domain)
>                            -- it c r_ (Constraint domain)

>        l_ <- subst l'                        -- update with current subst.
>        left_dp_type_ <- subst left_dp_type -- NB we extract range AFTER subst.

>        range <- case left_dp_type_ of    -- keep as monad, in case use fail_...
>                    (DP _ _ r) -> return r
>                    _          -> error "left_dp_type suddenly became not-DP!"

>        -- For Range: slight inefficiency, but reqd by depth-sensitive subst...
>        -- could bind with dummy binder for this op? 
>        -- this works anyway...

>        let result_ty = substitute range r'
>        let result = TT (Ap l_ r') result_ty

>        mtr ("AP out = " ++ show result)

>        ty_ch c (return result) mty




---
XXXX

THIS SHOULD DO STRICT CHECKING...


> it c (Co f a) 
>  = \mty ->
>    do 
>        (TT t ty) <- it c (Ap f a) mty
>        case t of 
>            (Ap l r) -> return (TT (Co l r) ty)
>            _        -> fail_with_msg $ "Co CTC hack failed, term " ++ show t


> it c t
>  = unknown_constructor "CTC it" t


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


> it_left_mvars c l r mty fail_msg 
>  | null $ subtrees is_new_metavar l
>     = fail_with_msg fail_msg 
>  where
>        is_new_metavar (Var (Plain ('?':_))) = True
>        is_new_metavar _                     = False

> it_left_mvars c l r No_Constraint fail_msg 
>  = fail_with_msg $ 
>                "Could not do anything about metavars in left branch:\n" 
>                ++ "  There is no downwards type constraint at this point."
>                ++ "Original fail message is:\n" ++ fail_msg
>        -- NEED TO CHECK BEFORE STACKING! 
>        -- OR change the fail constr type.

> it_left_mvars c l r (Coercible ety) fail_msg         -- TEST!
>  = it_left_mvars c l r (Constraint ety) fail_msg     -- EXPERIMENT!

> it_left_mvars c l r (Constraint ety) fail_msg 
>  = it_where "it_left_mvars" $ 
>    do
>        rt@(TT r' r_ty) <- it c r No_Constraint `try_another` fail_here

>        let fake_type = abstract_right c ety rt
>        mtr ("FAKE = " ++ show fake_type) 

>        -- make sure it is WF
>        tt@(TT fake' _) <- it c fake_type (Constraint Kind)    

>        tt@(TT l' l_ty) <- it c l (Constraint fake')
>        return tt
>    where
>        fail_here m
>         = fail_with_msg $ 
>                    "Left Metavars, no type constraint from right branch\n"
>                    ++ m ++ "\nOriginal fail message is:\n" ++ fail_msg


> abstract_right :: Ctxt -> Type -> TypedTerm -> Type
> abstract_right c ty (TT at aty)
>  = DP (Bound (Plain "v")) aty $ replace_term $ shiftUnderOneBinder ty
>    where
>        replace_term = replaceTerminal (is_replaceable (IVar db_start) at)

> is_replaceable t (ContextVar r)  (ContextVar n)  | r == n = Just t
> is_replaceable t (GlobalDef r _) (GlobalDef n _) | r == n = Just t
> is_replaceable t (IVar i)        (IVar j)        | i == j = Just t
> is_replaceable t (LocalDef i _)  (LocalDef j _)  | i == j = Just t
> is_replaceable t r               n                        = Nothing




%-------------------------------------------------------------------------------
AUX fns

> is_Kind_or_Nothing :: TypeConstraint -> Bool
> is_Kind_or_Nothing No_Constraint     = True
> is_Kind_or_Nothing (Constraint Kind) = True
> is_Kind_or_Nothing _                 = False


---

> it_err :: Ctxt -> String -> Term -> Type -> M a
> it_err c m t ty
>  = fail_with_msg $
>            m ++ "\n"
>            ++ showTermAndType c t ty ++ "\n"
>            ++ showCtxt (Show_Some 3) c ++ "\n"



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Surgery on types. 

Ap B 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Substitutions


> type MV_Info = (Name, Either () Ctxt_entry)

Repres via TT & Term? eg TT (MetaVar n) type), or GlobalDef... 

TypedTerm contains (for convenience) whether it is MetaVar or GlobalDef.

If (), then no solution. 

%---------------------------------------
Maintaining binding depth

> type MV_Ctxt = [[MV_Info]]
> init_MV_Ctxt = [[]] :: MV_Ctxt

---

> lookup_mv :: Name -> Ctxt -> MV_Ctxt -> Maybe TypedTerm
> lookup_mv n c mv_ss
>  = case findCtxt n c of
>        Ok cek -> Just $ translate_cek n cek
>        Fail _ -> case catMaybes foo of
>                    []                -> Nothing
>                    (l,Right cek) : _ -> Just $ shift l cek
>                    (l,Left _) : _    -> error "Left in lookup_mv"
>    where
>        foo = zipWith (fmap . (,)) [0..] (map (lookup n) mv_ss)

>        shift l cek
>         = let (TT t ty) = translate_cek n cek
>            in TT (shiftUnderSomeBinders l t) (shiftUnderSomeBinders l ty)


---

> show_mv_ss msg
>  = do
>        ss <- get
>        mtr (unlines $ msg : [ "  " ++ show s | s <- ss ])

---
BE GOOD TO INCLUDE SOME TREATMENT OF Bound tt
 - but not easy to do generically? 

 - NB mty needs to be shifted (or not) by the caller. (with shift_down)

> enter_binder             -- could use this route to putting ctxt in M? 
>  :: (Ctxt -> Ctxt) 
>     -> Ctxt
>     -> Term
>     -> MM TypedTerm
> enter_binder cf c t mty
>  = do
>        push_level 
>        t_ <- subst t
>        (TT t ty) <- it (cf c) t_ mty
>        drops <- pop_level c
>        let drops2 = [ (n, cek) | (n, Right cek) <- drops ]
>        let t'  = localiseDefs (Ctxt drops2) t
>        let ty' = localiseDefs (Ctxt drops2) ty
>        show_mv_ss "EXIT enter_binder ss"
>        return (TT t' ty')

> shift_down :: TypeConstraint -> TypeConstraint
> shift_down No_Constraint   = No_Constraint
> shift_down (Constraint ty) = Constraint $ shiftUnderOneBinder ty
> shift_down (Coercible ty)  = Coercible  $ shiftUnderOneBinder ty

---

> push_level :: M ()
> push_level = chg ([]:)

> pop_level :: Ctxt -> M [MV_Info]
> pop_level ctxt
>  = do
>        levels <- get
>        case levels of 
>            []         -> error "Empty MV_Ctxt during pop, should not occur"
>            [_]        -> error "pop_level - just one level left."
>            (l1:l2:ls) -> do
>                            let (drops, lifts) = drops_and_lifts l1
>                            mtr $ "DROPS = " ++ show drops
>                            mtr $ "KEEPS = " ++ show lifts
>                            check_no_dropped_mvars ctxt drops
>                            chg (\_ -> (lifts ++ l2) : ls) 
>                            return drops

SECONDARY DROPS get lifted once too much?     EH (may99)

MIGHT WANT TO DETECT M_Vs in the DROPS, and instantly fail? 


> check_no_dropped_mvars :: Ctxt -> [MV_Info] -> M ()
> check_no_dropped_mvars ctxt drops
>  = case [ stext n <+> colon <+> showTerm ctxt ty 
>          | (n, Right (CE (M_V _) ty)) <- drops ] of
>        [] -> return ()
>        ns -> fail_with_msg $ "Unsolved metavars when exiting binder:\n" 
>                                ++ longRender (vcat ns)


---

Iterate to remove dependencies.

> drops_and_lifts :: [MV_Info] -> ([MV_Info], [MV_Info])
> drops_and_lifts mvi 
>  | null iterated = initial_split
>  | otherwise     = (concat $ map fst $ reverse iterated, snd $ last iterated) 
>    where
>        initial_split = splitEither $ map lift_or_drop mvi
>        keep_dropping = iterate drop_again initial_split
>        iterated = takeWhile (\a -> not $ null $ fst a) keep_dropping

> drop_again (ds, ks)
>  = partition (\(n,Right cek) -> uses_ds $ translate_cek n cek) ks
>    where
>        ns = map fst ds
>        uses_ds (TT (GlobalDef _ d) ty) = ds_in_t ty || ds_in_t d
>        uses_ds (TT (MetaVar _)     ty) = ds_in_t ty 
>        ds_in_t t = ns `intersects` namesIn t
>        intersects as bs = not $ null $ intersect as bs


---

Left -> drop
Right -> keep (& lift)

> lift_or_drop :: MV_Info -> Either MV_Info MV_Info
> lift_or_drop mvi@(n, Right (CE (M_V atom) ty))
>  = Left mvi        -- always drop metavars.

THIS VERSION ALLOWED METAVARS TO FLOAT OUT WHILST THEIR TYPE IS VALID.
<>  = maybe (Left mvi) Right $ 
<>    do
<>        ty' <- shift_up ty
<>        return $ (n, Right $ CE (M_V atom) ty')

> lift_or_drop mvi@(n, Right (CE (Def d _) ty))
>  = maybe (Left mvi) Right $ 
>    do 
>        -- mtr $ "SHIFT DEF " ++ show (shift_up d)
>        d'  <- shift_up d
>        ty' <- shift_up ty
>        return $ (n, Right $ CE (Def d' $ GlobalDef n d') ty')

> lift_or_drop (n, _)
>  = error "lift_or_drop, bad cek or Left ()"

---

> shift_up :: Term -> Maybe Term 
> shift_up t
>  | null $ subtrees invalid_ref shifted = Just shifted
>  | otherwise                           = Nothing
>  where
>        shifted = shiftOverOneBinder t
>        invalid_ref (IVar i)       = i < db_start
>        invalid_ref (LocalDef i _) = i < db_start
>        invalid_ref t              = False



--- 

partition

if M_V without solution remains, then PROBLEM.
    maybe not? could backwards-fit? 
    Case of being mentioned elsewhere? ...  
    first-mention - then must be constrained there? 
    NB - esp if a plain var... - but type could help?  
        NB type of an expr also acts as a context, so it isn't purely textual
    Layering? - building pre-ctxt by declaring (and naming) all new vars 
        (new) at that level. (subsumes lower levels)
    Q: how far can a var escape - eg in type exprs - right to the top? 


if Defs then? shift in to parent - if can? - test for invalid IVar/Let?  



%---------------------------------------
add_ss
 - add a substitution, filtering out the old copy.

> add_ss :: MV_Info -> [[MV_Info]] -> [[MV_Info]]
> add_ss s0@(nm,_) []       = error "add_ss [], should not occur"
> add_ss s0@(nm,_) (ss:sss) = (s0 : filter (\nt -> fst nt /= nm) ss) : sss


---

subst needs to be valid at level i > level j. 
need subst OVER binders.

DOCUMENT! 

ss comes from lowest level.
then, apply to all higher levels, after shifting appropriately
using memo. technique to shift once each stage and iterate.

> update_mv_info :: Substitution -> [[MV_Info]] -> [[MV_Info]]
> update_mv_info ss mv_ss
>  = add_defs_to_lowest_level ss $ 
>    [map (update_ss ss') mv_s | (mv_s, ss') <- zip mv_ss shifted_ss ]
>    where
>        shifted_ss = map remove_bad shifts
>        shifts = iterate (fmap $ \(n,t) -> (n, t >>= shift_up)) 
>                         [ (n, Just t) | (n,t) <- ss ]
>        remove_bad ss = [ (n, t) | (n, Just t) <- ss ]

NB - bad idea for defs from cvt - we don't have types for these.
   - better to represent without type... (ie, replace Either type...)
   - in principle, we never use the type though? 

QQ: does (fold) order matter?

> add_defs_to_lowest_level ss mvss
>  = foldr (\(n,t) -> add_ss_with_type n t) mvss to_add
>    where
>        all_names = map fst $ concat mvss 
>        to_add = filter (\nt -> fst nt `notElem` all_names) ss 
>        dummy = ce_dummy_type "MVs - solution from unification"
>        add_ss_with_type n t 
>         = add_ss (n, Right $ CE (Def t $ GlobalDef n t) dummy)

---
update_ss
 - apply a Substitution
 - it might involve changing a MetaVar Name to a GlobalDef.

> update_ss :: Substitution -> MV_Info -> MV_Info
> update_ss ss s@(nm, Left ()) 
>  = s

> update_ss ss s@(nm, Right (CE mv@(M_V _) ty)) 
>  = let ty' = applySubstitution ss ty in 
>    case lookup nm ss of
>        Nothing  -> (nm, Right $ CE mv                           ty')
>        Just rhs -> (nm, Right $ CE (Def rhs $ GlobalDef nm rhs) ty')

> update_ss ss s@(nm, Right (CE (Def rhs _) ty)) 
>  = let rhs' = applySubstitution ss rhs in 
>    (nm, Right $ CE (Def rhs' $ GlobalDef nm rhs') (applySubstitution ss ty))

> update_ss ss s@(nm, Right (CE t ty)) 
>  = error $
>    "update_ss, internal error, metavar (" ++ show nm ++ "), cek is " ++ show t

---
`translate_cek'
  - get metavar value as a TypedTerm 

> translate_cek n (CE (M_V atom)   ty) = TT atom ty
> translate_cek n (CE (Def d atom) ty) = TT atom ty
> translate_cek n (CE cek ty)          = error $"translate_cek, : "++show cek 


---
`plain_substitution'
  - recovers the simple substitution inside the info. 

> plain_substitution :: [[MV_Info]] -> Substitution
> plain_substitution sss 
>  = let foo = plain_substitution_ sss in tr ("PLAIN: " ++ show foo ++ "\n") foo
> plain_substitution_ sss 
>  = [ (n, shiftUnderSomeBinders l t) 
>     | (l, ss) <- zip [0..] sss, (n, Right (CE (Def t _) _)) <- ss ]

---
`unify_with_mvs'
  - Add metavars from subst in to context, before unification
  - 
  - IMPORTANT - redo, eg CEK instead of either? 

> unify_with_mvs :: Ctxt -> Term -> Term -> M Substitution
> unify_with_mvs (Ctxt es) lt rt
>  = do
>        ss <- get
>        let es2 = foldr add_mv es $ concat ss
>        embed $ unifiable (Ctxt es2) lt rt
>    where
>        add_mv (n,Right ce@(CE (M_V _)   _)) es = (n,ce) : es
>        add_mv (n,Right ce@(CE (Def _ _) _)) es = (n,ce) : es
>        add_mv (n,Left _)                    es = es

%-------------------------------------------------------------------------------
Substitution Ops


---

`alloc_new_mvar'
  - get free name, 

LATER 
  - try allowing 2 levels of "?" for the unknown type?
  - ie, [?1 : ?2] (or El ?2 - maybe conv inserts?), and [?2 kind]
  - but, I think there is a problem with ambiguity - is it El-kind or other?

  - BUT - this may be solved by providing context? ie, Separate Kind/Term 
    syntax types (better), or extra flag (hack).
    IMPORTANT POINT! 

> handle_new_var c "?"
>  = \mty ->
>    do
>        nm <- get_new_name c first_numbered_metavar
>        handle_metavar c nm mty

> handle_new_var c ('?':n)
>  = \mty -> 
>    do
>        nm <- get_new_name c (Plain n)
>        handle_metavar c nm mty

> handle_new_var c n
>  = error $ "handle_new_var, no '?' in " ++ show n

---
TODO - use ty_ch

> handle_metavar :: Ctxt -> Name -> IM
> handle_metavar c nm mty
>  = it_where ("metavar " ++ show nm ++ ", " ++ show mty) $ 
>    do
>        si <- get
>        case lookup_mv nm c si of
>            Nothing  -> case mty of
>                        No_Constraint  -> fail_metavar nm
>                        Constraint ety -> do
>                                            let cek = CE (M_V $ MetaVar nm) ety
>                                            chg (add_ss (nm,Right cek))
>                                            return $ translate_cek nm cek
>                        Coercible ety  -> do
>                                            let cek = CE (M_V $ MetaVar nm) ety
>                                            chg (add_ss (nm,Right cek))
>                                            return $ translate_cek nm cek
>                            -- do identical to constraint case.
>            Just tt  -> ty_ch c (return tt) mty



---
`use_ss'
  - apply new subst where required
  - create new (or modified?) entry for given term. 

  - DANGER! can use bad substs...

> use_ss :: Substitution -> TypedTerm -> M TypedTerm
> use_ss ss (TT t ty)
>  = do
>        chg (update_mv_info ss)
>        show_mv_ss "USE out = "

<>        let tt = TT (applySubstitution ss t) (applySubstitution ss ty)

>        tt <- subst_tt (TT t ty)

>        mtr ("USE tt = " ++ show tt)
>        return tt



---

> fail_metavar :: Fallible m => Name -> m a
> fail_metavar (Plain n)
>  = fail_with_msg $ msg (if null n then "no-name" else n) 
>    where
>        msg m = "Can't create metavar (" ++ m ++ ") - no type information."

---
Generate fresh name.

> get_new_name :: Ctxt -> Name -> M Name 
> get_new_name c n0
>  = do
>        -- mtr ("GNN = " ++ n0 ++ "\n") 
>        ss_ns <- fmap (map fst . concat) get 
>        fn_gen <- get_free_names_generator
>        let ns = [ n | n <- fn_gen n0, n `notElem` ss_ns, isFail $ findCtxt n c]
>        return (head ns)

THIS ISN'T QUITE CORRECT - names could be bound in the local ctxt too.
added test for CC. find.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Running It.

Generic running of inference; can start from partially complete type info.A
Useful when processing (Maybe) type before the main term.

Might be an idea, though, to transfer MV_Info to Ctxt before next stage? 


---

NOT HAPPY WITH the initialisation from mty. 
can do better? 

> it_run :: FreeNameSupply -> Ctxt 
>                        -> Term -> Maybe Type -> OkF (MV_Ctxt, TypedTerm)
> it_run fns c t mty
>  = do
>        (s0, mty_) <- 
>            case mty of
>                Nothing   -> return (init_MV_Ctxt, No_Constraint)
>                Just Kind -> return (init_MV_Ctxt, Constraint Kind)
>                Just ety  -> do
>                                let it_kind = it c ety (Constraint Kind)
>                                (s0, TT t _) <- run_ST it_kind fns init_MV_Ctxt
>                                return (s0, Constraint t)
>        mtr ("it_run init s0 = " ++ show s0)
>        run_ST (it c t mty_) fns s0


---

External Function

Ctxt chg, plus showing. 

> type MV_Thing a = Name -> Ctxt_entry -> a -> OkF a

> inferType :: MV_Thing a -> FreeNameSupply -> Ctxt 
>                            -> Term -> Maybe Type -> a -> OkF (a, TypedTerm)
> inferType f fns c0 t mty a
>  = do
>        mtr ("ORIGINAL TERM:\n" ++ show t ++ "\n\n")
>        (ss,tt) <- it_run fns c0 t mty
>        mtr ("RESULTING SUBST:\n" ++ show ss ++ "\n\n")
>        let ss_ = case ss of
>                    []   -> error "no ss!"
>                    [ss] -> ss
>                    sss  -> error "multiple ss!"
>        a1 <- foldM (\a (n, Right ce) -> f n ce a) a $ reverse ss_
>        return (a1, tt)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
NOTES

EFFICIENCY
 - better to clear defs from state-subst for processing of RHS 
 - defs not needed since we apply the subst first
 - otherwise we end up repeating work.
 - MAYBE should split the notion of allocated names and actual substits.

IDEA:
 - ctxt and mty are DOWN. 
 - ctxt could be hidden - since it changes rarely.
 - mty should change a lot - so better visible.
 - new substs are UPWARDS.
 - NEW "MONADIC" TYPE FOR THIS? 

<> data M a = M Down Up Around a
<> return a = M undef neutral (id?) a
<> (M d u a x) >>= f = M 

need special ops for st mods?
 - eg merge_with_up
 - eg chg_around
 - eg replace_down


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
`findCoercion'
  - External Interface - checks that no new metavars result
  - to be used with non-metavar algorithms.

  - (TO EXPLAIN: that this process should not create new metavars)

> findCoercion :: Ctxt -> TypedTerm -> Type -> OkF (CoInfo, TypedTerm)
> findCoercion ctxt tt ty
>  = do
>        let run_M m = run_ST m (freeNamesOf ctxt) init_MV_Ctxt
>        let cs = coercions ctxt 
>        (ss, (ci, tt')) <- run_M $ find_coercion ctxt cs tt ty
>        assert_p (length ss == 1) (error "find_coercion returned invalid ctxt")
>        let unsolved = [ (n, ty) | (n, Right (CE (M_V _) ty)) <- head ss ] 
>        assert_p (null unsolved) (error "find_coercion - unsolved remain.")
>        return (ci, tt')

NB should mk abstract the mv_ctxt assertion? (local_ctxt and final_ctxt)


---

%---------------------------------------

> data Coercion 		-- use fields here!
>  = CO
>        CoercionName
>        Term    -- plain term. 
>        Term    -- ?-sat term.
>        Type    -- dom
>        Type    -- rng

> instance Show Coercion where
>	show (CO n _ _ _ _) = show n
 
> coercions :: Ctxt -> [Coercion]
> coercions (Ctxt es) 
>  = ctr ("reversing") $ reverse
>    [ CO n t mvt dom rng 
>     | (CoName n, ce@(CE (Coercion t mvt dom rng _ _) _)) <- es ]

NB CoName + Co isn't enforceable yet. 

---
Information to be collected in the graph

> data WhichCoercion
>  = BaseCoercion CoercionName
>  | RuleCoercion CoercionName WhichCoercion
>  | DP_Coercion 
>    deriving Show

> data CoInfo 
>  = CoInfo
>        WhichCoercion  -- base coercion
>        Term           -- actual term used
>        Term           -- domain
>        Term           -- range

> instance Show CoInfo where
>    show (CoInfo w t from to) = show w


<> mk_CoInfo :: CoercionName -> CoInfo
<> mk_CoInfo cn 
<>  = CI (BaseCoercion cn) nyk nyk nyk
<>    where
<>        nyk = Var "CO TERM NOT YET KNOWN"




%---------------------------------------

For selection, we want non-defeasible test. 
    possible to do so? 

---
`find_coercion'
  - main internal function. 
  - metavariables pass through
  - IMPORTANT: TT should NOT have the expected type, ie NO identity coercion...

  V2
  - 

> find_coercion 
>  :: Ctxt -> [Coercion] -> TypedTerm -> Type -> M (CoInfo, TypedTerm)
> find_coercion ctxt cs tt@(TT _ ty@(El _)) tgt@(El _)
>  = do
>       mctr $ "Looking for Type coercion from\n\t" ++ show_term ctxt ty
>                                ++ "\nto\n\t" ++ show_term ctxt tgt

>	ok_match <- choose_first (try_coercion ctxt cs tt tgt) cs
>	case ok_match of 
>	  Fail m   -> fail_with_msg "Find coercion failed."
>	  Ok ci_tt -> return $ snd ci_tt

---
anything else is a fail.

> find_coercion ctxt cs tt@(TT _ ty) tgt
>  = fail_with_msg $ 
>            "find_coercion, not El pair or DP pair"
>                ++ "\nSource: " ++ show_term ctxt ty 
>                ++ "\nTarget: " ++ show_term ctxt tgt

. 
%---------------------------------------

Need split operation
 - read state and write state passed out, but collect Ms afterwards.
 - but want permutation - 

 - ALTERNATIVE - manage own name supply. ??? 


> choose_first :: (b -> ST_OK s r a) -> [b] -> ST_OK s r (OkF (b,a))
> choose_first mf bs
>  = ST_OK 
>  $ \ r s -> case [ (b,result)
>                  | (b,ST_OK fn) <- [ (b,mf b) | b <- bs ]
>                  , let out = fn r s
>                  , isOk out || ctr ("Fail: " ++ fromFail out) False
>                  , let (Ok result) = out ] of
>               []          -> return $ (s, Fail "No solution in choose_first")
>               (b,(s,a)):_ -> return $ (s, Ok (b,a))




%---------------------------------------


<> -- find_coercion_aux :: ... -> M OkF 
<>  :: Ctxt -> [Coercion] -> TypedTerm -> Type -> M (CoInfo, TypedTerm)

> try_coercion ctxt cs tt@(TT _ ty@(El _)) 
>                     tgt@(El _) 
>                         (CO n ct mvt dom rng) 
>  = ctr ("Trying " ++ show n ++ ", "  ++ show_term ctxt ct
>         ++ "\n\tDOM " ++ hack_show dom 
>         ++ "\n\tRNG " ++ hack_show rng 
>         ++ "\n\t----"
>         ++ "\n\tsrc " ++ show_term ctxt ty 
>         ++ "\n\ttgt " ++ show_term ctxt tgt
>         ++ "\n" ++ indent_show "\tct  " (hack_show ct)
>         ++ "\n\tmvt " ++ hack_show mvt
>         ) 
>  $ do
>       tt <- try_coercion_aux ctxt cs mvt tt tgt
>       mctr $ "Found match,\n" ++ show tt
>       return (CoInfo (BaseCoercion n) mvt ty tgt, tt) 



> try_coercion_aux ctxt cs co_plus_mvs tt@(TT arg arg_ty) tgt_ty
>  = do
>       old_mv_info <- get 
>		-- necessary? - yes, as check that we only solve new
>		-- not sure [feb02] after code rearrangement
>       mtt@(TT m mt) <- it ctxt co_plus_mvs No_Constraint
>	  -- fix the obvious parameters

>	-- pass info from target type to coercion range.
>       let (co_dom,co_rng) = case mt of DP _ ty t -> (ty,t)
>       let new_rng = substitute co_rng arg

-- choice of which to do first - does it matter? I think range usu better

>       mctr ("attempt Co range unify: " 
>		++ "\n\t" ++ show_term ctxt tgt_ty
>		++ "\n\t" ++ show_term ctxt new_rng ++ "\n")
>       m_ss_rng <- lift $ unify_with_mvs ctxt tgt_ty new_rng
>       mctr ("Co ranges unified: " ++ show m_ss_rng)

>	-- apply ss to etc.
>	let chg_term = case m_ss_rng of
>	                 Fail _ -> id
>	                 Ok ss  -> applySubstitution ss 
>	let arg_ty2 = chg_term arg_ty
>	let co_dom2 = chg_term co_dom
>       mctr ("attempt Co domain unify: " 
>		++ "\n\t" ++ show_term ctxt arg_ty2
>		++ "\n\t" ++ show_term ctxt co_dom2 ++ "\n")
>       m_ss_dom <- lift $ unify_with_mvs ctxt arg_ty2 co_dom2
>       mctr ("Co domains unified: " ++ show m_ss_dom)

>	let total_ss = concat $ okfToList m_ss_rng ++ okfToList m_ss_dom

>	-- apply subst to metavar term and its type, passing info back down
>       new_mtt <- use_ss total_ss mtt
>       tt2 <- apply_to_TT Co ctxt (return new_mtt) tt
>		-- now update the term being coerced
> --

>	-- clean up missing details.
>       solve_dp_parameters ctxt cs old_mv_info
>	-- this means various cuts are done. 

>	case m_ss_rng of	-- now try dom unify
>		Fail _ -> do
>	                    tgt_ty2  <- subst tgt_ty
>	                    new_rng2 <- subst new_rng
>	                    ss_rng <- unify_with_mvs ctxt tgt_ty2 new_rng2
>	                    chg (update_mv_info ss_rng)
>		Ok _   -> return ()

>	case m_ss_dom of	-- now try dom unify
>		Fail _ -> do
>	                    arg_ty3 <- subst arg_ty2
>	                    co_dom3 <- subst co_dom2
>	                    ss_dom <- unify_with_mvs ctxt arg_ty3 co_dom3
>	                    chg (update_mv_info ss_dom)
>		Ok _   -> return ()

>	-- force in any new details
>       tt3@(TT x _) <- subst_tt tt2
> --
>       mctr ("Co apply accepted: " ++ show_term ctxt x) 
>       return tt3



%---------------------------------------
`find_unsolved_dps'
  - do subtraction at this binding level to find new metavars.
  - then filter out the ones which have DP kind.

> find_unsolved_dps :: MV_Ctxt -> M [(Name,Type)]
> find_unsolved_dps [] 
>  = error "find_unsolved_dps, null"

> find_unsolved_dps (old_info:_)
>  = do
>        let old_mvs = [ (n, ty) | (n, Right (CE (M_V _) ty)) <- old_info ]
>        let old_nms = map fst old_mvs
>        new_info <- fmap head $ get
>        let new_mvs = [ (n, ty) | (n, Right (CE (M_V _) ty)) <- new_info ]
>        let added = filter (\nty -> fst nty `notElem` old_nms) new_mvs

>        return $ reverse $ filter (is_coercion_form.snd) added

> is_coercion_form ty 
>  | null bs   = False
>  | otherwise = isEl (snd $ last bs) && isEl matrix 
>  where
>        (bs,matrix) = un_DP ty


%-------------------
MISNAME 

> solve_dp_parameters :: Ctxt -> [Coercion] -> MV_Ctxt -> M ()
> solve_dp_parameters ctxt cs old_mv_ctxt
>  = do
>        unsolved <- find_unsolved_dps old_mv_ctxt
>        mctr ("UNSOLVED:\n" 
>	        ++ unlines [ "\t" ++ show n ++ " : " ++ show_term ctxt t 
>	                   | (n,t) <- unsolved ])
>        mapM_ (solve_one_parameter ctxt cs) unsolved


%-------------------
`solve_one_parameter'

- here, are trying to clean up the current parameter. 
- NB - we should iterate, since later could help earlier??? 

Type is (xs:Ps)S -> T
Q is, does any co use 

eg (x:A)List (B x) < List (B' x)

aim, co z : 

If just aiming one type, then can't do it.


> solve_one_parameter :: Ctxt -> [Coercion] -> (Name,Type) -> M ()
> solve_one_parameter ctxt cs nty@(n,ty@(DP _ _ _))
>  = do
>       ty' <- subst ty
>       mctr ("O_P in: " ++ show n ++ " : " ++ show_term ctxt ty')
>	(TT t _) <- solve_one_parameter_aux ctxt cs ty'
>	let t2 = etaContract t
>       mctr ("O_P out: " ++ show n ++ " : " ++ show_term ctxt t2)
>	chg (update_mv_info [(n,t2)])

AIM: using the co. mechanism..., but avoid IT! 
NB this going under binders means problems with IVars...

> solve_one_parameter_aux ctxt cs (DP v ty rty)
>  = fmap (\(TT t_ ty_) -> TT (FO new_v ty t_) (DP new_v ty ty_)) 
>  $ do
>       push_level 
>       -- t_ <- subst t
>       (TT t ty) <- solve_one_parameter_aux (addCtxt_BV v ty ctxt) cs rty
>       drops <- pop_level ctxt
>       let drops2 = [ (n, cek) | (n, Right cek) <- drops ]
>       let t'  = localiseDefs (Ctxt drops2) t
>       let ty' = localiseDefs (Ctxt drops2) ty
>       show_mv_ss "EXIT enter_binder ss"
>       return (TT t' ty')
>    where
>	new_v = case v of 
>		  Underscore -> Bound $ Plain "x"
>		  bv         -> bv

---

> solve_one_parameter_aux ctxt cs rty
>  = ctr ("SP_aux " ++ show_term ctxt rty
>         ++ "\n"
>         )
>  $ do
>	(TT t ty) <- it ctxt first_IVar (Constraint rty)
>	let t2 = case t of { Co l r -> Ap l r; t -> t }
>	  -- change top-level term to normal application node
>	  -- this is to avoid confusion at later stage.
>	  -- alternatively: the above uses a trick to calculate the coercion
>	  --   and the above undoes the trick.
>	return $ TT t2 ty

-- here. 



> indent_show s ls
>  = unlines $ zipWith (++) (s' : repeat [' ' | _ <- s']) (lines ls)
>    where
>	s' = concat $ map (\c -> case c of { '\t' -> "        "; _ -> [c]}) s
> 
