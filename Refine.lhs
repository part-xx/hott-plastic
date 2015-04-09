> module Refine (refine, Name, SimpleTerm, IM) where

Performs Refine operation. 

> import Data.List(sortBy)
> import Control.Monad(foldM)

> import Base
> import Terms
> import Context
> import SimpleToTerm(SimpleTerm, translate_db)
> import ContextOps

> import Printing(showCtxt)
> import Debugging(show_term, trace)
> import GlobalOptions(p_Normal, p_Everything)

> -- import IntrosReturn (intros)

> import Convertibility(convertible)
> import TypeInference(inferType)
> import Reduction(hnf)

> import MetaVars(refine_MetaVars)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
REFINE

 - get ctxt of name, with type, and check it is Hyp
 - check validity of term in reduced context, and get its type.

 - meta-ise the term, against expected type. this produces a subctxt of new
   new metavars.

 - put these in to context before the current entry. 

NB need to ensure that MVs are NOT clashing with full context! 
pass a predicate based on full ctxt? 
or rename these? 
XXXX to be done.

 - then cut in the constructed refine term
   ASSUME that meta_ise cuts the metavars in this term anyway! 
   WHICH IT PROBABLY DOESN'T!!!


> refine :: CtxtReference -> SimpleTerm -> IM
> refine i_n t
>  = do
>		c <- get_M_ 
>		n <- embed $ getReference i_n c
>		-- print_M_ $ "Refine on: " ++ show (n,t)
>		refine_ n t


IDEA: multi-ctxt clarifies position of "post-defs", since they will be in
non-dependent section of ctxt, so can be left in place and the context-join be
performed after ReturnIntros? 

> refine_ :: Name -> SimpleTerm -> IM
> refine_ n t
>  = do
>		c_orig <- get_M_
>		(CE kind ty) <- embed $ findCtxt n c_orig
>		assert_is_hyp_or_mv kind 
>                           ("Bad Refine Name - not a hypothesis: " ++ show n)

>		t1 <- use_M_ $ \c -> translate_db c t

<>		s_t2 <- use_M_ (\c -> Ok $ show_term c_pre t2) 
<>		print_M_ $ "REF:\n" ++ s_t2 ++ "\n"
<>		print_M_ $ "REF2 = " ++ pretty_crude_print t2 ++ "\n"

COULD JUMP IN HERE FOR Induction TACTIC.
THERE SHOULDN'T BE ANY POST-DEFS, since expr is WF at MV and WE control the
intros sequence.

>		let refine_op = refine_MetaVars (freeNamesOf_fast c_orig) t1 ty
>		(chgs,t2) <- contextChanges refine_op

>		set_M_ c_orig		-- should be this anyway...

add unsolved metavars in to context, before the refine target.
NB what about localising their dependencies? TODO! 
EXPAND in place the metavar solutions since they are purely LOCAL.
Also, don't want to see them in the result.

>		t3 <- case chgs of
>				Just (decls, pre_defs, post_defs)
>				 -> do
>					chg_M_ (insertBefore n decls)	-- make metavar changes.
>					chg_M_ (cutBefore n pre_defs)	-- make metavar changes.
>					-- let t3 = localiseDefs ((\(PostDefs x) -> x) post_defs) t2
>					let t3 = expandSomeDefs post_defs t2
>					return t3
>				Nothing -> return t2

>		c_mv <- get_M_
>		p_Everything "Term becomes:\n"
>		p_Everything $ show_term c_mv t3 

>		prepend_msg "When cutting in the Refine term, got a problem:" $
>				chg_M_ (internal_cut n (\_ -> Ok t3))	-- and cut the new term.

