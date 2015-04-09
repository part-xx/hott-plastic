> module PrintingAux (
>			pp_term
>			,runPM, PM, State
>			,BinderFormat(..), show_binder
>			,CustomPrinter, P_Function, P_Info(..) 
>		) where

Pretty-printing of terms - basic facilities 

> import IOExts_(unsafePerformIO)

> import Data.Maybe(isJust,fromJust)
> import Pretty hiding (TextDetails)
> import PrettyAux(stext)

> import Base
> import ReaderMonads

> import Terms
> import SharedSyntax
> import TermOps(db_start, isFirstIVarUsed)
> import Context(Ctxt, ce_dummy_type)
> import Context(addCtxt_Let, addCtxt_BV, findCtxt_DB, findCtxt)

> import GlobalOptions(PP_Options(..), prettyPrint_Options)

#ifndef __HASKELL98__
#define fmap map
#endif

%-------------------------------------------------------------------------------
Bracketing and Layout Notes

Style: 
	- allowing CURRIED where possible. 
	- maybe have style switchable.
	- try to reduce ( ) 

	- maybe vertical layout for big expressions.

%-------------------------------------------------------------------------------
NOTES:

still have to implement Let.

use ap-squashing - see what the args are
	THOUGH might want to generalise and have Ap :: Term -> [Term] -> Term.

Put this inside Context.lhs for simplicity? (eg allowing showContext to use it)

Could move showContext to here - but this means extending visibility of the
	Context internal representation

Name usage: 
	ctxt addition renames ALWAYS - to avoid confusion.
	but in $printing$ we can reuse the same name...
	maybe we should have OPTION to squash or not.


EXTENSION			-- EH EH? 
	- [x]x x x 
	- AS LONG AS 'x' doesn't occur inside this expr from elsewhere.
	- this means, complex occurrence check? 
		- But could do cyclically!  

Free Var Detection
 - is crude - fairly expensive. 
 - could do better! 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
`PM' - Monad for the printing process.
 - keeps a value for the custom printing function.
 - have to wrap the RM because of the recursion with CustomPrinter (grrr)
 - MORE HERE

> type State = (Ctxt, CustomPrinter, PP_Options)

> newtype PM a = PM ( RM State a )

<> {-#SPECIALISE instance Monad ((->) State) #-}

> extract_PM (PM m) = m
> through_PM f (PM m) = PM $ f m

> instance Functor PM where
>	fmap f (PM m) = PM $ fmap f m

> instance Monad PM where
>	return a     = PM $ return a
>	(PM m) >>= k = PM $ m >>= \x -> extract_PM (k x)

> runPM (PM m) = runRM m 

> useStatePM :: (State -> a) -> PM a
> useStatePM = PM . useStateRM

---
The Monad `methods' add bound variables to context (for when we enter a binder)
and then retrieve the uniquified name used by the context.

> chg_ctxt :: (Ctxt -> Ctxt) -> PM a -> PM a
> chg_ctxt cf
>  = through_PM $ chgStateRM (map_fst3 cf) 

> get_name :: DB_Index -> PM Name
> get_name i = useStatePM (fst . findCtxt_DB i . fst3)


---
`pp_opt' 
  - access to underlying opt, as boolean test.

> pp_opt :: (PP_Options -> Bool) -> PM Bool
> pp_opt f = fmap f $ useStatePM thd3

---
`pp_switch'
  - choice on boolean pp opt.

> pp_switch :: (PP_Options -> Bool) -> PM Doc -> PM Doc -> PM Doc
> pp_switch flag yes no
>  = do
>	pgm <- pp_opt flag
>	if pgm then yes else no

---
Instantiations

> if_mark :: (Doc -> Doc) -> PM Doc -> PM Doc
> if_mark f md = pp_switch markClosedTerms (fmap f md) md


> pg_switch :: Doc -> Doc -> PM Doc 
> pg_switch yes no = return no
> -- DISABLED pg_switch yes no = pp_switch proofGeneral (return yes) (return no)
> -- some people didn't like it...


---
quick hack for turning off the (quick hacks for) sugaring
 - used when pure output is required.

> {-# NoInline hack_server_mode #-}
> hack_server_mode :: Bool
> hack_server_mode = unsafePerformIO $ fmap serverMode prettyPrint_Options


---
`CustomPrinter'
  - is a hook for customised printing routines (ie, for "user syntax")
  - allow user to specify how to print certain terms.
  - the Maybe indicates whether user wishes to rewrite or not. 

  - newtype is needed to avoid synonym recursion (grrr)

> type CustomPrinter = P_Function (Maybe Doc)
> custom_printer :: PM CustomPrinter
> custom_printer = useStatePM snd3

---
`P_Function'
  - generic type for prettyprinting functions (here, or user's)

> type P_Function r  = P_Info -> Term -> PM r


---
`P_Info' 
  - indicates "context" of printing for a term.
  - from this we decide whether to use ( ) or not.

> data P_Info 
>  = No_Info		-- default conventions.
>  | Arrow_LHS		-- does need () 
>  | Ap_LHS		-- L assoc, so doesn't need () 
>  | Ap_RHS		-- does need ()
>  | Binder_Type	-- A in [x:A]B
>  deriving (Show, Eq)

---
common pattern

> need_parens :: P_Info -> Doc -> Doc
> need_parens Ap_RHS = parens 
> need_parens _      = id

---
global settings for nesting

> nested, nest_binder, nest_args :: Doc -> Doc
> nested      = nest 2     -- for anything
> nest_binder = id         -- for binding forms
> nest_args   = nest 1     -- for arguments

%---------------------------------------
Idioms

> t_ :: String -> PM Doc
> t_ = return . text

> ts_ :: Show a => a -> PM Doc
> ts_ = t_ . show


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Main Code

---
`pp_term'
  - main function here.

> pp_term :: P_Function Doc
> pp_term pi t
>  = do
>	cpf <- custom_printer
>	m <- cpf pi t		-- see if user wants rewrite.
>	case m of
>		Nothing -> st pi t	-- doesn't, so print as normal.
>		Just d  -> return d	-- does, and this is the doc.



%-------------------------------------------------------------------------------
`st'
  - this function does the real work.

> st :: P_Info -> Term -> PM Doc

> st _ (Kind)          = t_ "kind"		-- should be caught before show? 
> st _ (Type)          = t_ "Type" >>= underlined 

> st _ (Elim i _)      = ts_ i 
> st _ (Const i _)     = ts_ i 
> st _ (F_Elim i)      = if_mark (<> (text "_F"))   $ ts_ i 
> st _ (F_Const i _)   = if_mark (<> (text "_F"))   $ ts_ i 
> st i (Closed t _)    = if_mark ((text "<CT>") <>) $ pp_term i t 

> st _ (GlobalDef i _) = ts_ i 
> st _ (MetaVar v)     = t_ $ "?" ++ show v
> st _ (ContextVar v)  = ts_ v
> st _ (Var v)         = ts_ v		-- WAS: "(V " ++ v ++ ")", 
>							-- but may occur in non-db translated terms.

> st _ (IVar i)        = get_name i >>= \n -> ts_ n
> st _ (LocalDef i _)  = get_name i >>= \n -> ts_ n



Recursive Cases.

> st i (El t)
>  = do
>	show <- pp_opt showEl
>	if show 
>	  then do
>		t' <- pp_term Ap_RHS t       -- treat arg like RHS of an Ap.
>		return $ text "El" <+> t'
>	  else do
>		pp_term i t

Might need flag here, for showing the El.


> -- special case, for my sanity! 
> -- should be flagged too.
> -- special treatment of El in binder is required. 
> st i (Ap (Ap (ContextVar nm@(Plain tc)) _) (FO v (El ty) t))
>  | tc == "Pi" || tc == "Sigma"
>  = do
>	ty' <- pp_term No_Info ty
>	(n',t') <- chg_ctxt (addCtxt_BV v (El ty)) $
>		   do
>			n' <- get_name db_start	 -- see what n has become
>			t' <- pp_term No_Info t
>			return (show n', t')
>	return $ 
>		enclose_binder i $ 
>		text tc <> cat [ hcat [char '[', text n', colon, ty', char ']']
>					   , t' ]


> st i (Ap (Ap (ContextVar nm@(Plain tc)) _) 
>          (FO v (El ty) t))
>  | tc == "FA"
>  = do
>	ty' <- pp_term No_Info ty
>	(n',t') <- chg_ctxt (addCtxt_BV v (El ty)) $
>		   do
>			n' <- get_name db_start	 -- see what n has become
>			t' <- pp_term No_Info t
>			return (show n', t')
>	return $ 
>		enclose_binder i $ 
>		cat [ hcat [char '{', text n', colon, ty', char '}']
>			  , t' ]

---
LIMITED CASE: and want proper P & A.

> st i (Ap (Ap f l) r) 	-- LIMITED.
>  | isJust op && fromJust op `elem` ["=>", "==>"]
>  = do
>	l' <- pp_term Arrow_LHS l
>	r' <- pp_term No_Info r
>	return $ parens $ sep [l', text (fromJust op), r']
>    where
>	op = isOp $ nameFromTerm f
>	isOp (Just (Operator op)) = Just op
>	isOp _                    = Nothing

> st i ap@(Ap _ _)
>  = do
>	fn'   <- pp_term Ap_LHS fn
>	args' <- mapM (pp_term Ap_RHS) args
>	return $ enclose $ sep (fn' : map nest_args args')
>    where
>	(fn,args) = flattenAp ap

>	enclose | i == Ap_RHS                = parens
>	        | isBinder fn && i /= Ap_LHS = parens
>	        | otherwise                  = id

> st i (Co t1 t2)
>  = do
>	show_co <- pp_opt showCoercions
>	if not show_co
>	  then do			-- don't show them
>			pp_term i t2
>	  else do			-- SHOULD BE EXACT COPY OF ABOVE, MODULO EFFECTS
>			t1' <- pp_term Ap_LHS t1
>			t2' <- pp_term Ap_RHS t2
>			rev_t1 <- reversed t1'
>			return $ enclose $ sep [rev_t1, nested t2']
>    where
>	enclose | i == Ap_RHS                = parens
>	        | isBinder t1 && i /= Ap_LHS = parens
>	        | otherwise                  = id





---

> st i (Let v t1 t2)
>  = show_let_binder i v t1 t2

> st i (DP v ty t)
>  = show_binder i fo_binder [(v,ty)] t

> st i t@(FO v ty body)
>  = show_binder i fl_binder [(v,ty)] body
>    -- where (bs,body) = un_FO t

---

> st i (Ext (SoftCast e t))
>  = do
>	e' <- pp_term No_Info e
>	t' <- pp_term No_Info t
>	return $ parens $ e' <+> text "::" <+> t'

> st i (Ext (HardCast e t))
>  = do
>	show_t <- pp_opt showCAST 
>	if show_t 
>		then do
>			e' <- pp_term No_Info e			-- parentheses regardless.
>			t' <- pp_term No_Info t			-- don't pass expr-ctxt info
>			bold_e <- in_bold e'
>			return $ parens $ bold_e <+> text ":!:" <+> t'
>		else do
>			e' <- pp_term i e	-- pass expr-ctxt info
>			in_bold e' 		-- highlight it.

---

> st i (Ext e)
>  = return $ 
>	text "EXTENSION NOT COVERED YET" $$ 
>	text (show e)
 
---
Catch-all.

> st i t
>  = unknown_constructor "st" t

%-------------------------------------------------------------------------------

> data BinderFormat
>  = BinderFormat 
>	(Maybe (String,String)) 	-- start and end brackets, if applicable
>	(Maybe String)				-- arrow symbol, if applicable.

> fo_binder = BinderFormat (Just ("(", ")")) (Just "->")
> fl_binder = BinderFormat (Just ("[", "]")) Nothing

---

> show_binder :: P_Info -> BinderFormat -> [Binding] -> Term -> PM Doc

> show_binder i (BinderFormat Nothing Nothing) ((n,ty):ntys) t
>  = error $ "Double Nothing in show_binder: " 
>		++ show n ++ "\n\t" ++ show_some ty ++ "\n\t" ++ show_some t
>    where
>	show_some = take 70 . show 

%-------------------
NOTES
<>	args' <- mapM (pp_term Ap_RHS) args
<>	return $ enclose $ sep (fn' : map (nest 2) args')

foldM (\(v,ty) -> chg_ctxt for TAIL.

<> foo i ((n,ty):ntys) t
<>  = do
<>	ty' 

Problem if unused vars.
	-> segment by this?, rather than pure FO. 
	->  

%-------------------

> show_binder i (BinderFormat bs a) [(n,ty)] t
>  = do 
>	ty' <- pp_term type_p_info ty
>	(n',t') <- chg_ctxt (addCtxt_BV n ty) $
>	    	   do
>			n' <- get_name db_start		-- see what n has become
>			t' <- pp_term No_Info t
>			return (show n', t')
>	return $
>		enclose_binder i $ 
>		if var_free && isJust a || not (isJust bs)
>		then sep [ty', nest_binder $ sep [text $ fromJust a, t']]
>		else cat [hcat [text s, text n', colon, ty', text e], nest_binder t']
>    where
>	var_free = not $ isFirstIVarUsed t
>	type_p_info = if var_free then Arrow_LHS else Binder_Type
>	(s,e) = fromJust bs

---
`show_let_binder' - simplified version of above.

> show_let_binder :: P_Info -> Name -> Term -> Term -> PM Doc
> show_let_binder i n rhs t
>  = do 
>	rhs' <- pp_term No_Info rhs
>	(n',t') <- chg_ctxt (addCtxt_Let n rhs $ ce_dummy_type "pp let") $
>    		   do
>			n' <- get_name db_start		-- see what n has become
>			t' <- pp_term No_Info t
>			return (show n', t')
>	return $
>		enclose_binder i $ 
>		cat [hcat [open, text n', equals, rhs', close], nest_binder t']
>    where
>	(open,close) = (text "[", text "]")


---


> enclose_binder Ap_LHS    = parens
> enclose_binder Ap_RHS    = parens
> enclose_binder Arrow_LHS = parens
> enclose_binder _         = id


---
VT 220 Codes for text effects.

should have a flag for these - esp if not using terminal IO! 

> esc = "\27" 

> underline_on = text $ esc ++ "[4m"
> underline_off = text $ esc ++ "[m"

NOTE: pg_switch is disabled.

> underlined d 
>  = pg_switch d		--  was (annoying): (text "_" <> d <> text "_")
>              d		-- (underline_on <> d <> underline_off)

> bold_on  = text $ esc ++ "[1m"
> bold_off = text $ esc ++ "[m"

> in_bold d 
>  = pg_switch d		-- was (annoying): (text "|" <> d <> text "|")
>              d		-- (bold_on  <> d <> bold_off)

> reverse_on  = text $ esc ++ "[7m"
> reverse_off = text $ esc ++ "[m"

> reversed d 
>  = pg_switch d		-- was (annoying): (text "<<" <> d <> text ">>")
>              d		-- (reverse_on <> d <> reverse_off)

---


