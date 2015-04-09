> module InductiveAux (
>			buildElimRule,
>			buildLiftConstrs,
>			CompRule,
>			Declaration,
>			SimpleTerm,
>			Name,
>			e_name,
>			c_name,
>			Relation_or_what(..)

>			, un_bs		-- TEMPORARY
>			) where

Aux stuff for implementing Luo's scheme for Inductive Families.

#if __GLASGOW_HASKELL__ < 302
> import Trace		-- TMP! 
#else 
> import IOExts_(trace)
#endif

> import Base

> import SimpleTerm
> import SharedSyntax(BindVariety(..), Name(..), BoundName(..), unbind)
> import CommandTypes(CompRule(..), Declaration(..), Definition(..))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

This contains building blocks for calculating the Elim rule for an inductive
family from the schemata.

LATER: Also calculates various useful theorems? (Based on Elim?)

---

Implementation: series of tests which return Fail or an Ok containing the
information that we want to extract from the term, eg for a SPO (strictly
positive operator), the bindings and the arguments to X.

This information is then used in creating the Elim type and Elim rules.

These are ALL syntactic checks and manipulations. Validity is enforced by
requiring that the resulting terms are well-typed and legal in the context
where they are introduced. 

%-------------------------------------------------------------------------------
IMPORTANT

some of these checks are best done on the kinds of the constrs AFTER they have
been through type-synthesis, thus ELs are made explicit etc.
(lesser note: this could permit coercions in the definitions...)

We get some confirmation through the constructors being declared in ctxt, 
so major details like small-kind test are probably sufficient as syntactic 
checks.

NEED THINK.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
`buildElimRule' builds the Elim rule.

#ifndef __HASKELL98__
#define sequence accumulate
#endif

> import DebugOptions(traceOnDebugFlag)

> lift_trace m = traceOnDebugFlag "LIFT_TRACING" (m `fseq` m)

> lift_trace_m m = lift_trace m $ return ()

> buildElimRule
>  :: Relation_or_what
>  -> Name
>  -> SimpleType
>  -> [Declaration] 
>  -> [Declaration]
>  -> OkF (Declaration, Arity, [CompRuleFunction])
> buildElimRule is_rel name kind params constrs
>  = do
>		fam_bindings <- has_valid_kind is_rel kind
>		let ps_bindings = parameter_bindings params

>		let mk_is = inductive_schema is_rel name ps_bindings (length fam_bindings) 
>		is_info <- sequence [ mk_is n ty | (Declaration [n] ty) <- constrs ]

>		let e_type  = construct_E_type is_rel name ps_bindings fam_bindings is_info
>		-- trace ("ET: " ++ show e_type ++ "\n") $ return ()
>		let e_arity = length $ fst $ un_DP_S e_type
>		let e_nm = e_name name 
>		let c_nm = c_name name
>		let c_fn = mk_c_expr is_rel (Name_S c_nm)
>		let e_rules = compile_E_rules c_nm c_fn e_nm ps_bindings e_type is_info

>		return $
>			(Declaration [e_nm] e_type
>			, e_arity
>			, e_rules)


---
Standardising these names:

> type E_Name = Name

> e_name (Plain n) = Plain $ "E_" ++ n :: E_Name

> f_name :: BoundName -> BoundName
> f_name (Bound (Plain n)) = Bound $ Plain $ "f_" ++ n

> type Nm_Binding = (Name, SimpleType)

---
C-name and C-expr construction

> type C_Name = Name 
> c_name :: Name -> Name
> c_name (Plain n) = Plain $ "C_" ++ n :: C_Name


> data Relation_or_what    -- indicates how to build C-exprs
>  = Standard   
>  | Relation 
>  | Relation_LE

NB poss hide the c_name thing inside here? 

> nProp = Name_S $ Plain "Prop"       -- need parametrise?
> nType = Name_S $ Plain "Type"       -- ditto?

---
`mk_c_type'
  - form the type of the C_x family.
  - for Prop, it is a Prop depending ONLY on the family arguments.

> mk_c_type :: Relation_or_what -> [DP_Binding] -> SimpleType -> SimpleType
> mk_c_type Relation bs _  
>  = un_bs bs $ nProp               -- (z1:P1)..(zm:Pm)Prop
> mk_c_type Relation_LE bs _  
>  = un_bs bs $ nType               -- (z1:P1)..(zm:Pm)Type
> mk_c_type Standard bs mu_expr
>  = un_bs bs $ ndp mu_expr nType   -- (z1:P1)..(zm:Pm)Mu(z1-zm)Type


---
`mk_c_expr'
  - forms application of the C_x family
  - for Prop, you don't need the constructed value (see above)

> type C_Expr_Fn = [SimpleTerm] -> SimpleTerm -> SimpleTerm

> mk_c_expr :: Relation_or_what -> SimpleTerm -> C_Expr_Fn
> mk_c_expr Relation c_name ps _ 
>  = Ap_S (Name_S $ Plain "Prf") (ap_ts c_name ps)   -- Prf(C(p1..pm))

> mk_c_expr Relation_LE c_name ps _ 
>  = ap_ts c_name ps                                 -- Prf(C(p1..pm))

> mk_c_expr Standard c_name ps z
>  = ap_ts c_name $ ps ++ [z]                        -- C(p1..pm,I(x1..xn))




*************** QQ: need smallness conditions on the other args? 


%---------------------------------------

---
un_DP_S' decomposes a term into the DP bindings and the end term.

> un_DP_S :: SimpleType -> ([DP_Binding], SimpleType)
> un_DP_S (Bind_S By_DP n ty t) = let (nts, e) = un_DP_S t in ((n,ty):nts,e) 
> un_DP_S t                     = ([], t)

<> un_bs :: [DP_Binding] -> SimpleType -> SimpleType
<> un_bs bs e = foldr (\(n,ty) t -> Bind_S By_DP n ty t) e bs

<> un_bs_FO :: [DP_Binding] -> SimpleType -> SimpleType
<> un_bs_FO bs e = foldr (\(n,ty) t -> Bind_S By_FO n ty t) e bs

---
`ndp' create a non-dependent product, ie a DP with name Underscore
 
> ndp :: SimpleType -> SimpleType -> SimpleType
> ndp ty t = Bind_S By_DP Underscore ty t

---
Checks there are no underscores.
 - TODO [22aug01] - find a better way to enforce this check.

> apNames :: BoundName -> [BoundName] -> SimpleTerm
> apNames (Bound nm) ns
>  | Underscore `elem` ns
>     = error $ "InductiveAux.apNames - Underscore in bindings:\n"++show ns
>  | otherwise 
>     = ap_nms nm [ n | (Bound n) <- ns ]

> apNames_ :: Name -> [BoundName] -> SimpleTerm
> apNames_ nm = apNames (Bound nm)


---


---
params to the inductive family.

> type IF_Binding = DP_Binding			


---
`add_parameters'
  - add parameters to use of given nm.

> add_parameters :: [IF_Binding] -> Name -> SimpleTerm -> SimpleTerm
> add_parameters ps_bs nm = replaceName_S nm (apNames_ nm $ map fst ps_bs) 

---
Bindings for the parameters.

> parameter_bindings :: [Declaration] -> [IF_Binding]
> parameter_bindings ds
>  = [ (Bound n, ty) | (Declaration ns ty) <- ds, n <- ns ]



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
`has_valid_kind' - SYNTACTIC CHECK that kind is a dep prod, ie has form
	:: (z1:P1)....(zm:Pm)Type
Returns list of bound vars with their types.

NOTES:
	Are there any particular restrictions on P_i? maybe have to check that 
	it is well-typed? (ie, it is a kind). Or let the type checker do this? 

> has_valid_kind :: Relation_or_what -> SimpleTerm -> OkF [DP_Binding]
> has_valid_kind is_rel t
>  = if has_required_kind is_rel e 
>      then return bs
>      else fail_with_msg $ "Not valid kind " ++ show t
>    where
>		(bs, e) = un_DP_S t

---
[23aug01] - don't like this - too untidy.

> has_required_kind :: Relation_or_what -> SimpleTerm -> Bool
> has_required_kind Standard (Name_S (Plain "Type")) 
>  = True
> has_required_kind Relation_LE t
>  = has_required_kind Relation t
> has_required_kind Relation (Ap_S (Name_S (Plain "El")) 
>                                  (Name_S (Plain "Prop")))
>  = True
> has_required_kind _        _ = False

---
To avoid deriving Eq for SimpleTerm - test this particular case.

> sameName (Name_S x) (Name_S y) = x == y
> sameName _          _          = False

%-------------------------------------------------------------------------------
`small_kind'

NOTE:
     just syntactic check - might want to have reduction? 
     A: NO! since it is a kind; there are NO FUNCTIONS on kinds.

     see notes above about processing (concrete) SimpleTerms or abstract Terms

> small_kind :: SimpleType -> Bool
> small_kind (Bind_S By_DP _ ty k)          = small_kind ty && small_kind k
> small_kind (Ap_S (Name_S (Plain "El")) _) = True
> small_kind (Ap_S _ _)                     = False
>  -- non-El application expression, see note on El omission
> small_kind (Pars_S k)                     = small_kind k  -- syntactic marker
> small_kind k
>  = error $ "small kind, unrecognised construct: " ++ show k


%-------------------------------------------------------------------------------
`strictly_positive' 

:: (x1:K1)...(xn:Kn)X(p1...pm)
K_i is small kind, X ni FV(K_i), 
pj has kind with deps up to Pj
	-- isn't this given by the expression being typable? (ie, as a Kind?)

> type IF_Name  = Name			-- for "X"
> type IF_Arity = Int			-- for m in p1..pm
> type IF_Arg   = SimpleTerm	-- for p_i

> data SPO = SPO [DP_Binding] [IF_Arg] deriving Show

> strictly_positive 
>  :: Relation_or_what -> IF_Name -> IF_Arity -> SimpleType -> OkF SPO 
> strictly_positive is_rel x a t
>  | not all_small_kinds 
>     = fail_with_msg "contains non-small-kind"
>  | x_occurs
>     = fail_with_msg "X occurs in SPO arguments"
>  | otherwise
>     = do
>          args <- check_arity x a (strip_prf is_rel e)
>          return $ SPO bs args
>  where
>		(bs, e) = un_DP_S t
>		all_small_kinds = all small_kind $ map snd bs
>		x_occurs = any (x `occursIn`) $ map snd bs 

---
decide whether to remove/replace "Prf"

> add_prf Relation t    = Ap_S (Name_S (Plain "Prf")) t 
> add_prf Relation_LE t = add_prf Relation t
> add_prf Standard t    = t

> strip_prf Relation (Ap_S (Name_S (Plain "Prf")) t) = t
> strip_prf Relation t                               = t
> strip_prf Relation_LE t                            = strip_prf Relation t
> strip_prf Standard t                               = t
> -- strip_prf w t = error $ "SP: " ++ show t

---
`check_arity' - checks that term `e' is an applic of name `x' to `a' args.

> check_arity x a e
>  = do
>		as <- args_of x e
>		if (length as == a) 
>			then return as
>			else fail_with_msg (msg $ length as)
>    where
>		msg l = "SPO arity, expect " ++ show a ++ ", got " ++ show l 

> args_of x (Pars_S t) 
>  = args_of x t
> args_of x (Ap_S t a) 
>  = do
>		as <- args_of x t
>		return $ as ++ [a]
> args_of x (Name_S n) 
>  | x == n    = return []
> args_of x t 
>  = fail_with_msg $ "args_of error (" ++ show x ++ ") for: " ++ show t



%-------------------------------------------------------------------------------
`inductive_schema' checks for term being a dep prod, with M_i being a small
kind with X free, or a SPO. Also checks arity of X applic and returns the
arguments.

:: (x1:M1)...(xn:Mn)X(p1...pm) 

> type SPO_Binding = (BoundName, SPO)		-- bound with arg it represents

> type IS_Arg = SimpleTerm
> data IS
>  = IS BoundName [IF_Binding] [DP_Binding] [SPO_Binding] [IS_Arg] 

name of IS
parameters
all bindings
all spo-bindings (subset of all bindings)

> inductive_schema 
>  :: Relation_or_what -> IF_Name -> [IF_Binding] 
>                              -> IF_Arity -> Name -> SimpleType -> OkF IS
> inductive_schema is_rel x ps_bs a nm t
> -- ps_bs: Parameters
>  | not small_kinds_ok
>     = fail_with_msg $ 
>			"an argument was neither small kind nor SPO\n" 
>				++ unlines (map show not_spos)
>  | x_occurs_in_a_small_kind
>     = fail_with_msg $ "a small kind contained X"
>  | otherwise
>     = do
>          args <- check_arity x a (strip_prf is_rel e)
>          return $ IS (Bound nm) ps_bs (add_ps bs) (add_ps_spo spos) args
>  where
>		(bs,e) = un_DP_S t
>		(not_spos,spos) = splitEither $ map separate_spo bs 
> 
>		small_kinds_ok = all small_kind $ map snd not_spos
>		x_occurs_in_a_small_kind = any (x `occursIn`) $ map snd not_spos

>		add_ps     bs = [ (n, add_parameters ps_bs x ty) | (n,ty) <- bs ]
>		add_ps_spo bs = [ (n, SPO (add_ps bs) if_as) | (n, SPO bs if_as) <- bs ]

>		separate_spo :: DP_Binding -> Either DP_Binding SPO_Binding
>		separate_spo b@(n,m)
>		 = elimOk (const $ Left b) (\sp -> Right (n,sp)) $ 
>		   strictly_positive is_rel x a m




%-------------------------------------------------------------------------------
`construct_E_type'

> construct_E_type 
>  :: Relation_or_what	-- special treatment needed? 
>     -> IF_Name 		-- X
>     -> [IF_Binding] 	-- global parameters. 
>     -> [DP_Binding] 	-- bindings for X, from has_valid_kind
>     -> [IS]			-- list of inductive schemata
>     -> SimpleType		-- the result.

> construct_E_type is_rel x ps_bs bs is
>  | not $ all_vars_bound (ps_bs ++ bs)
>     = error $ "construct_E_type - Underscore in bindings:\n"
>               ++ show (ps_bs ++ bs)
>  | otherwise
>  = un_bs (
>		ps_bs 
>		++ [(Bound c_nm, c_type)]
>		++ [(f_name nm, is_type c_fn i) | i@(IS nm _ _ _ _) <- is]
>		++ bs 
>		++ [(Bound z_name, add_prf is_rel mu_expr)]
>		)
>		c_result
>    where
>		c_nm = c_name x
>		z_name = Plain "z"
>		c_fn = mk_c_expr is_rel (Name_S c_nm)

>		c_type = mk_c_type is_rel bs mu_expr 
>		                             -- (z1:P1)..(zm:Pm)Mu(z1-zm)"Type/Prop"
>		mu_expr  = ap_nms x [ n | (Bound n, _) <- ps_bs ++ bs ]
>		                             -- X(a1..ak, z1..zm)
>		c_result = c_fn [ Name_S n | (Bound n, _) <- bs ] (Name_S z_name)
>		                             -- C(z1..zm, z)
>		all_vars_bound = all is_bound . map fst
>		is_bound (Bound _)  = True
>		is_bound Underscore = False


---
`is_type' is theta_i^o

> is_type :: C_Expr_Fn -> IS -> SimpleType
> is_type c_name_fn (IS nm ps_bs bs spos as)
>  = un_bs (bs ++ map (spo_bd c_name_fn ps_bs) spos) c_result
>    where
>		c_result = c_name_fn as iota                    -- C(p1..pm,I(x1..xn))
>		iota     = apNames nm $ map fst $ ps_bs ++ bs	-- I(x1..xn)

---
`spo_bd' is phi_i^o. 
	This is exactly like theta_i^o, but there are no SPOs in the term.
	Get the IS name from the param that SPO is bound with.
	NB no dependence, hence the bind name is Underscore

> spo_bd 
>  :: C_Expr_Fn 
>  -> [IF_Binding] 
>  -> SPO_Binding 
>  -> (BoundName, SimpleType) 
> spo_bd c_name_fn ps_bs (n, SPO bs as) 
>  = (Underscore, is_type c_name_fn (IS n [] bs [] as))


%-------------------------------------------------------------------------------
`compile_E_rules' does just that, first constructing them and then converting
in to conventional functions.

> type E_Prefix = SimpleTerm

> compile_E_rules 
>  :: C_Name
>  -> C_Expr_Fn
>  -> E_Name
>  -> [IF_Binding] 
>  -> SimpleType 
>  -> [IS] 
>  -> [CompRuleFunction]
> compile_E_rules c_name c_name_fn e_name ps_bs e_type iss
>  = -- trace ("E_TERM = " ++ show e_term ++ "\n") $ 
>    map (compile_E_rule c_name_fn e_pre_bs e_term) iss
>    where
>		fns = [ f_name n | (IS n _ _ _ _) <- iss ] :: [BoundName]
>		e_bs = fst $ un_DP_S e_type
>		e_term = apNames_ e_name $ map fst ps_bs ++ Bound c_name : fns
>		e_pre_bs | length e_bs > 0 = init e_bs
>		         | otherwise       = error "Empty E_Type Binding List"

---
`compile_E_rule' is in two parts: form the basic expression, and then convert
it to a function.

> compile_E_rule 
>  :: C_Expr_Fn -> [DP_Binding] -> E_Prefix -> IS -> CompRuleFunction
> compile_E_rule c_name_fn e_pre_bs e_term is
>  = -- trace_crf $  
>    compile_rule e_pre_bs $ 
>    construct_rule c_name_fn e_term is
>    where
>		trace_crf x@(CRF n a t ty) 
>		 = trace (unlines $ ["CR " ++ show (n,a), show t, show ty]) x

---
`ICR' holds the info expected by rule compilation (saves decomposing the
terms).

> data ICR
>  = ICR {	is     :: IS,
>			x_args :: [SimpleTerm],
>			c_args :: [BoundName],
>			rhs    :: SimpleTerm,
>			kind   :: SimpleTerm}

> construct_rule :: C_Expr_Fn -> E_Prefix -> IS -> ICR
> construct_rule c_name_fn e_prefix is@(IS nm ps_bs bs spos as)
>  = ICR is as constr_args rhs kind		-- HERE
>    where
>		constr_args = map fst $ ps_bs ++ bs

>		normal_args = map fst bs
>		c_expr = apNames nm constr_args -- normal_args
>		kind = c_name_fn as c_expr
>		rhs = apNames (f_name nm) normal_args `ap_ts` recursive_args
>		recursive_args = map (phi_nat e_prefix) spos

---
`phi_nat' forms the term for a recursive argument, ie:
	where phi_i_j == (y1:N1)...(yl:Nl)X(q1...qm)
	phi_i^# = [y1:N1]...[yl:Nl]( (E(C,f1...fn)) q1...qm, xij(y1...yl) )

> phi_nat :: E_Prefix -> SPO_Binding -> SimpleTerm
> phi_nat e_prefix (n, SPO bs as)
>  = un_bs_FO bs $ ap_ts e_prefix $ as ++ [apNames n $ map fst bs]


JOIN sections.

%---------------------------------------
Compilation of Comp Rules from Inductive Schemata

Special form for inductive rules.
	- because we have special treatment for the pattern matching.

---
`compile_rule' 

This function will take the Elim args, plus the args of the constructor.
	(thus its arity depends on the constr).

NB: binding occs of names are as from IS and E type.
	- hence, constr bindings get the natural names, whereas the args from
		X(p1..pm) are taken from the decl of X. 

	- which means, there is no confusion here over nonlin patterns, since
		names will be unique 

NOTE:
	should explain my conjecture here! XXXX

> compile_rule :: [DP_Binding] -> ICR -> CompRuleFunction
> compile_rule e_pre_bs (ICR (IS nm ps_bs bs _ as) xas cas rhs kind)
>  = CRF (hack_nm nm) arity f_term f_type
>    where
>		arity  = length cas
>		f_bs   = e_pre_bs ++ [(Underscore, ty) | (_,ty) <- ps_bs ] ++ bs
>		f_term = un_bs_FO f_bs rhs
>		f_type = un_bs    f_bs kind

---
WHAT TO DO?

> hack_nm Underscore = Plain "_"
> hack_nm (Bound nm) = nm 



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Robert's monadic stuff.

NOTES:
  handling of names needs to be fixed.
  eg clarify whether the schemas should produce bound names or not.


> construct_E_lift
>  :: IF_Name 			-- X
>     -> [IF_Binding] 	-- global parameters. 
>     -> [DP_Binding] 	-- bindings for X, from has_valid_kind
>     -> [IS]			-- list of inductive schemata
>     -> SimpleType		-- the result.

> construct_E_lift x ps_bs bs is
>  = un_bs (
>		ps_bs 
>		++ [(Bound c_nm, c_type)]
>		++ [(f_name nm, is_type (error "NYI-cel") i) | i@(IS nm _ _ _ _) <- is]
>		++ bs 
>		++ [(z_name, mu_expr)]
>		)
>		c_result
>    where
>		c_nm = c_name x
>		c_type = un_bs bs $ ndp mu_expr nType	-- (z1:P1)..(zm:Pm)Mu(z1-zm)Type
>		mu_expr  = apNames_ x $ map fst $ ps_bs ++ bs 	    -- X(a1..ak, z1..zm)
>		c_result = apNames_ c_nm (map fst bs ++ [z_name])	-- C(z1..zm, z)
>		z_name = Bound $ Plain "z"

> buildLiftConstrs
>  :: Name-> SimpleType-> [Declaration] -> [Declaration]-> 
>								(OkF [Definition])
> buildLiftConstrs name kind params constrs
>  = do

>		fam_bindings <- has_valid_kind (error "NYI blc") kind
>		let ps_bindings = parameter_bindings params

>		let mk_is2 :: Name -> SimpleType -> OkF IS2
>		    mk_is2 = inductive_schema2 name ps_bindings (length fam_bindings) 

>		is_info2 <- sequence [ mk_is2 n ty | (Declaration [n] ty) <- constrs ]
>               -- is_info2 :: [IS2]

>		let mk_is = inductive_schema (error "NYI blc2") name ps_bindings (length fam_bindings) 
>		is_info <- sequence [ mk_is n ty | (Declaration [n] ty) <- constrs ]
>		let e_lift  = construct_E_lift name ps_bindings fam_bindings is_info


>		sequence_ [lift_trace_m $ "One IS:\n" ++ show is | is <- is_info2 ::[IS2]]

<>		     defs = []

>	 	defs <- sequence (map (lift_def_from_is name) is_info2)

>		sequence_ [lift_trace_m $ "Name: " ++ show n ++ "\nTerm: " ++ show t | (n,t) <- defs]

>		return [ (Definition [n] [] term Nothing) 
>		        | (n,term) <- ((unliftName name,Name_S name):defs)]


> lift_def_from_is :: Name -> IS2 -> OkF (Name,SimpleTerm)
> lift_def_from_is name (IS2 cname params ises args) = 

<>			 return ("TTT_" ++ cname, Name_S name)

>	 return (unliftName cname, term)

>	 where

>	  is_to_name (IS_DP (n,_)) = unbind n
>	  is_to_name (IS_SPO (n,_)) = unbind n

>         mk_res_ty dps = ap_ts (Name_S (unliftName name)) dps
>	  res_ty = mk_res_ty args
>	  res_te = ap_ts (Name_S cname) (map (Name_S . is_to_name) ises)

>	  return_term = ap_ts return_name [res_ty, res_te]
>	  binds_term = foldr addone return_term ises
>           where addone (IS_DP _) prev = prev
>	          addone (IS_SPO (n,SPO [] if_args)) prev = 
>		    ap_ts bind_name [mk_res_ty if_args, res_ty, fo_s n (mk_res_ty if_args) prev, (Name_S $ unbind n)]

>	  term = foldr addone binds_term ises
>	    where addone (IS_DP (n,t)) prev = fo_s n t prev
> 		  addone (IS_SPO (n, SPO [] if_args)) prev = 
>		    fo_s n (Ap_S monad_name (mk_res_ty if_args)) prev
>	  fo_s = Bind_S By_FO

> monad_name, bind_name, return_name :: SimpleTerm
> return_name = Name_S $ Plain "return" 
> bind_name   = Name_S $ Plain "bind" 
> monad_name  = Name_S $ Plain "M" 

<>	  term2 = un_bs_FO (map is_to_dp ises) (Ap_S (Ap_S (Name_S "return") res_ty) res_te)

<>	  fin = ap_ts (Name_S "return") [ap_ts (Name_S name) args, Name_S cname]
<>	  addone :: ([(Name,SimpleType)], SimpleTerm) -> IS_Binding -> ([(Name,SimpleType)], SimpleTerm)
<>	  addone (tys,t) (IS_DP (n,tt)) = ((n,tt) : tys, FO_S (Just n) tt (Ap_S t (Name_S n)))
<>	  addone (tys,t) (IS_SPO (n,SPO [] [])) = ((n,(Name_S name)) : tys, FO_S (Just n) (Name_S name) (Ap_S t (Name_S n)))

<>	  (types,term) = foldl addone ([], fin) ises


> unliftName :: Name -> Name
> unliftName (Plain ('L' : '0' : '_' : name)) = Plain $ "L_" ++ name


----

> data IS_Binding = IS_DP DP_Binding | IS_SPO SPO_Binding
> data IS2
>  = IS2 Name [IF_Binding] [IS_Binding] [IS_Arg] 

> instance Show IS_Binding where
>	show (IS_DP dp) = "IS_DP " ++ show dp
>	show (IS_SPO spo) = "IS_SPO " ++ show spo

> instance Show IS2 where
>       show (IS2 n fbind sbind sargs) = "IS Name: " ++ show n ++ "\n" 
>		++ "   FBind: " ++ show fbind ++ "\n"
>		++ "   SBind: " ++ show sbind ++ "\n"
>		++ "   SArgs: " ++ show sargs ++ "\n"
>		

> inductive_schema2
>  :: IF_Name -> [IF_Binding] -> IF_Arity -> Name -> SimpleType -> OkF IS2
> inductive_schema2 x ps_bs a nm t
> -- ps_bs: Parameters
>  | not small_kinds_ok
>     = fail_with_msg $ 
>			"an argument was neither small kind nor SPO\n" 
>				++ unlines (map show not_spos)
>  | x_occurs_in_a_small_kind
>     = fail_with_msg $ "a small kind contained X"
>  | not $ isOk arity_ok
>     = rethrow arity_ok
>  | otherwise
>     = return $ IS2 nm ps_bs (map add_one_ps bindings) (fromOk arity_ok)
>  where
>		(bs,e) = un_DP_S t
>		bindings :: [Either DP_Binding SPO_Binding]
>		bindings = map separate_spo bs 
>		(not_spos,spos) = splitEither $ bindings
> 
>		small_kinds_ok = all small_kind $ map snd not_spos
>		x_occurs_in_a_small_kind = any (x `occursIn`) $ map snd not_spos
>
>		arity_ok = check_arity x a e

>		add_ps     bs = [ (n, add_parameters ps_bs x ty) | (n,ty) <- bs ]
>		add_ps_spo bs = [ (n, SPO (add_ps bs) if_as) | (n, SPO bs if_as) <- bs ]

>		add_one_ps :: Either DP_Binding SPO_Binding -> IS_Binding
>		add_one_ps (Left (n, ty)) = IS_DP (n, add_parameters ps_bs x ty)
>		add_one_ps (Right (n, SPO bs if_as)) = IS_SPO (n, SPO (add_ps bs) if_as)

>		separate_spo :: DP_Binding -> Either DP_Binding SPO_Binding
>		separate_spo b@(n,m)
>		 = elimOk (const $ Left b) (\sp -> Right (n,sp)) $ 
>		   strictly_positive (error "NYI-is2") x a m

