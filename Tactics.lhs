> module Tactics (
>				induction
>				, immed_tactic
>				, equiv_tactic
>			) where

Misc smaller tactics

-- SHOULD RE-EXPORT Refine? And the other main tactics? 


#if __GLASGOW_HASKELL__ < 302
> import Trace
#else 
> import IOExts_(trace)
#endif

<> import List(sortBy)
<> import Monad(foldM)

> import Base
> import Terms
> import TermOps(un_DP)
> import SimpleTerm (SimpleTerm, mk_Name_S)
> import Context
> import IntrosReturn (intros)
> import SimpleToTerm(SimpleTerm, translate_db)
> import ContextOps
> import Debugging
> import GlobalOptions(p_Normal)

> import Convertibility(convertible)
> import TypeInference(inferType, inferKind)
> import Reduction(dwhnf_to_ap)
> import InductiveAux(e_name, c_name)

> import Refine(refine)

> import MetaVars(refine_MetaVars)

> import SharedSyntax(IdOrNum(..))



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Non-primitive tactics 
 = those do-able via other tactics
 - eg induction can be done with clever calls to refine.
 - idea is that we don't have complex routines? is this ok? 

 - BUT - some tactics have to return info (eg var renamings) which we need.
 - ABANDON!



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
`immed_tactic'
  - try to solve the referenced metavar by Refine on something in the ctxt.
  - if no specific name given, then loop until we get a failure.

	NEED TO INCLUDE Meta-Refine.

> immed_tactic :: CtxtReference -> IM
> immed_tactic r@Nothing
>  = do
>		is <- while_M_ immed_tactic_once (repeat r)
>		p_Normal $ "Repeat Immed - solved (" ++ show (length is) ++ ") vars"
>		return ()

> immed_tactic r = immed_tactic_once r


	TECHNICALLY, should only attempt to refine a term in its sub-ctxt? 
	some working of post-defs should also be done...
		- but this comes more easily when context redesigned? 


> immed_tactic_once :: CtxtReference -> IM
> immed_tactic_once r 
>  = do
>		(Ctxt es) <- get_M_
>		did_one <- until_loop $ map (attempt_refine r . fst) es
>		if did_one
>			then do
>					p_Normal "Immed: a refine succeeded"
>			else do
>					p_Normal "Immed: No workable refines from the context"
>					fail_with_msg "immed failed"

	---
	lift failure to refine to visible Bool.

> attempt_refine :: CtxtReference -> Name -> CM Bool
> attempt_refine r n
>  = catch_M_ (\_ -> return False) $
>    do
>		refine r (mk_Name_S n) 
>		p_Normal "REFINE WAS OK"
>		return True

	---

> until_loop :: Monad m => [m Bool] -> m Bool
> until_loop [] 
>  = return False
> until_loop (x:xs)
>  = do
>		f <- x
>		if f
>			then return f
>			else until_loop xs

	 

* target <- use_M_ (getReference target_ref)	-- WARNING -mightn't be MVar


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Equiv.

rewrite type of reference with term, if they are convertible kinds.

also expands post-definitions that maybe in the new term 
	(NB should maybe move def back? NOT PROBLEM for multi-ctxt...)

> equiv_tactic :: CtxtReference -> SimpleTerm -> IM
> equiv_tactic r st
>  = do
>	tgt <- use_M_ $ getReference r
>	p_Normal $ "Equiv on target: " ++ show tgt
>	(_, CE _ ty, post_defs) <- use_M_ $ findNameInCtxt tgt

>	(TT _ _) <- use_M_ $ \c -> inferKind c ty	-- an assertion.

>	t <- use_M_ $ \c -> translate_db c st
>	(TT t' _) <- use_M_ $ \c -> inferKind c t

<>	t' <- get_M_ >>= \c -> refine_MetaVars (freeNamesOf c) t ty_ty

>	use_M_ $ \c -> convertible c ty t'
>	let new_type = expandSomeDefs post_defs t'
>	chg_M_ $ chgCtxt tgt (\(CE cek _) -> Ok $ CE cek new_type)
>	p_Normal "Done."


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Induction tactic

simple version of lego's stuff.

extension to do sensible thing when naming a term-to-apply-to? 
	maybe have problem of not knowing exact arity of E_X? 

	easier if just fill in C_X? but still have to count args before C_X
	IT SHOULD BE OK. DO IT.

	BUT - standardised name, means C_ is reliable? CHECK Sol! OR C_X like lego?!
	should be doable? 



%---------------------------------------
`expose_var'
  - do intros until var to be inducted-on is in the ctxt.
  - it could be in the ctxt already.

  - CARE: ambiguity: an 'x' in context, and an 'x' in binding. Ctxt wins.
  - but, this means that we can repeat intros until we do get it in context
		(and there is NO possibility of renaming, since no shadow in ctxt!)

  - returns the name of the var to be inducted over.

> expose_var :: Name -> IdOrNum -> CM (Name, Type)
> expose_var = error "expose_var: not yet defined"

#if 0
> expose_var target nm@(Left n)
>  = name_and_type n 
>    `try_M_` \_ ->
>		do 
>			intros (Just $ Left target) (return ()) [Underscore]
>			expose_var target nm

> expose_var target (Right 0)
>  = fail_with_msg "Induction, must have index > 0"

> expose_var target (Right n)
>  = do
>		v <- intros (Just $ Left target) (return ()) [Underscore]
>		case n of
>			1   -> name_and_type v
>			m+2 -> expose_var target (Right $ m + 1)

---

> name_and_type :: Name -> CM (Name, Type)
> name_and_type n
>  = do
>		(CE _ ty) <- use_M_ $ findCtxt n
>		return (n,ty)

#endif

%-------------------
`determine_inductive_type'
  - reduce type to an application of some inductive family constructor to
	some arguments.

> determine_inductive_type :: Type -> CM (Name, [Term])
> determine_inductive_type ty
>  = case (dwhnf_to_ap ty) of
>		Ok (ContextVar n, ts) -> return (n,ts)
>		Ok (_           , ts) -> fail_with_msg msg
>		Fail _                -> fail_with_msg msg
>    where
>		msg = "Couldn't determine_inductive_type " ++ crude_print ty

BUT sense of elim op too! 

WHAT IF A FAMILY? EXPERIMENT? 



%-------------------

add single metavar.
 - working in abs syn
 - fake the adding. NO unification needed (?) COULD BE! 
 - use the claim function with show? then have a named metavar.
	[ COR: can use the claim function anyway? 

but con

> add_single_metavar :: Type -> CM (OkF Type)
> add_single_metavar = error ""

- find first binding.
claim it.
then want to have a term (E ?m_i) which is NAMED.
Refine_term? - or need a vreg.


%-------------------
`decode_e_args'
  - given name of inductive type, get the bindings and separate in to
	(pre_c, c_bind, post_c, z_type), where
		c_bind is the type of the arg constructing the result type
		pre_c are the bindings before it
		post_c are the bindings after it
		z_bind is the binding of the arg-to-be-inducted-on.

  - the DB vars are converted (in un_DP) to (Var n). 
	Is this likely to cause a problem? 

> decode_e_args :: Name -> CM ([Binding], Binding, [Binding], Binding)
> decode_e_args nm
>  = do
>		let e_nm = e_name nm
>		let c_nm = Bound $ c_name nm

>		CE _ e_ty <- use_M_ (findCtxt e_nm)
>		let (bs, e_range) = un_DP e_ty

>		let pre_c = takeWhile (\b -> fst b /= c_nm) bs
>		assert_p (length bs >= length pre_c + 2) (malformed_e_type e_nm bs)
>		let c_bind = bs !! length pre_c
>		let post_c = init $ drop (length pre_c + 1) bs
>		let z_bind = last bs

>		return (pre_c, c_bind, post_c, z_bind)

> malformed_e_type n bs
>  = "Bad E_X type for " ++ show n
>		++ unlines [ "\t" ++ show n ++ ":" ++ crude_print ty | (n,ty) <- bs ]


%-------------------

> induction :: CtxtReference -> IdOrNum -> IM
> induction target_ref id_or_num
>  = atomicM_ $ 
>    do
>		target <- use_M_ (getReference target_ref)	-- WARNING -mightn't be MVar
>		
>		(iv,iv_ty) <- expose_var target id_or_num

>		(ind_type, ind_args) <- determine_inductive_type iv_ty

#if 0
---------------
List etc etc.

>		(pre_c, c_bind, post_c, z_bind) <- decode_e_args ind_type

Now, pre_c will be bound by the first (length pre_c) arguments from ind_args,
and these args are bound purely in context. Substitute these in now.

>		let e_nm = e_name ind_type

>		sh_ind_args <- use_M_ (\c -> map (showTerm_ c) $ take (length pre_c) ind_args

>		let e_type = parseTermThenCall Ok $ unwords $ e_nm ++ sh_ind_args


> ---
>		let foo m bs = unlines $ m : ["\t"++n++":"++crude_print ty|(n,ty)<-bs]
>		print_M_ $ show (iv,e_nm) ++ " == " ++ crude_print iv_ty ++ "\n" ++ foo "pre_c" pre_c ++ foo "c_bind" [c_bind] ++ foo "post_c" post_c ++ foo "z_bind" [z_bind] ++ "\n"
> ---

NB use same-metavar notation to help solution? 

>		CE _ tgt_ty <- use_M_ (findCtxt target)


<>		let c_term = FO "_" iv_ty tgt_result


<>		let refine_term = SHOWING! build_ap elim_nm [ metavar | _ <- pre_C ]

"?" build the refine term.

<>		call refine proper.		- BUT NOT the simple term bit! 
<>		PROBLEM: 

			show everything as text, and construct the argument line? 
			seems a bit crude.
			difficulty is with metavar handling. 

#endif

>		print_M_ "END\n\n"


%---------------------------------------
Lego Notes

see recent docs from conor.

%---------------------------------------



Goal nat -> nat ; Intros; Induction H; GETS

   Refine by  [Phi:nat->nat]Phi H
      ?2 : nat->nat
   Refine by  nat_elim ([_:nat]nat)
      ?3 : nat
      ?4 : nat->nat->nat

WHY? 

CONCL: better to leave the inducted var in place? 


%---------------------------------------
value = Pi_elim
type  =
  {A:Type}{B:A->Type}{C_Pi:(Pi A B)->TYPE}({x1:{x:A}B x}C_Pi (pi A B x1))->
  {z:Pi A B}C_Pi z
Lego> Goal {A:Type}{B:A->Type}(Pi A B) -> Pi A B;
Goal 
   ?0 : {A:Type}{B:A->Type}(Pi A B)->Pi A B
Lego> Induction 3;
Error: there aren't that many (..)-> premises
Lego> Induction 2;
Error: there aren't that many (..)-> premises
Lego> intros A B;
intros (2) A B
  A : Type
  B : A->Type
   ?1 : (Pi A B)->Pi A B
Lego> Induction 1;
Refine by  Pi_elim A B ([_:Pi A B]Pi A B)
   ?2 : ({x:A}B x)->Pi A B

SO - lego is ok on pre C_Pi arguments, AND it also fills them in.
%---------------------------------------
