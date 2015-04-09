> module Printing (
>			showTerm, 
>			showTerm_,
>			showTypedTerm,
>			showTypedTerm_,
>			showTermAndType,

>			showCtxt,
>			showInCtxt,
>			CtxtShow(..),
>			-- showCtxtEntry,

>			longRender,
>			shortRender,
>			Doc
>			) where

Pretty-printing of terms

> import IOExts_(unsafePerformIO)
> import Data.List(mapAccumL, tails)
> import Data.Maybe(isJust, catMaybes)

> import Pretty
> import PrettyAux

> import Base
> import Terms
> import TermOps(namesIn)
> import TermReduction(reduceAppliedAbstrs)
> import Context
> import ReaderMonads(RM)

> import PrintingAux
> import GlobalOptions(prettyPrint_Options, PP_Options(..))

<> import UserSyntax(showUserSyntax)



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Showing Terms.

> {-# NoInline showTerm #-}
> showTerm :: Ctxt -> Term -> Doc
> showTerm c t 
>  = let options = unsafePerformIO prettyPrint_Options
>    in runPM (pp_term No_Info $ reduce options t) 
>          (c, no_user_syntax, options)
>    where
>	reduce os = if reduceTerms os then reduceAppliedAbstrs [] else id 

> no_user_syntax _ _ = return Nothing


> showTerm_ :: Ctxt -> Term -> String
> showTerm_ c = longRender . showTerm c


> showTypedTerm :: Ctxt -> TypedTerm -> Doc
> showTypedTerm c (TT t ty)
>  = text "TT" <+> vcat [showTerm c t, showTerm c ty]

> showTypedTerm_ :: Ctxt -> TypedTerm -> String
> showTypedTerm_ c = longRender . showTypedTerm c


---
`showTermAndType' - show term with its type.

> showTermAndType :: Ctxt -> Term -> Type -> String
> showTermAndType c t ty
>  = longRender $
>    text "VAL" <+> ((equals <+> showTerm c (value_of t))
>					 $$
>					 colon_if_not_kind ty (showTerm c ty))
>    where
>		value_of (GlobalDef x d) = d
>		value_of t               = t

---
`colon_if_not_kind' - special case for showing kinds.

> colon_if_not_kind Kind pp = text " " <> pp
> colon_if_not_kind _    pp = colon <+> pp



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Showing the context

CtxtShow indicates what to show.

> data CtxtShow 
>  = Show_Entry Name 		-- show a single entry
>  | Show_Detailed Name		-- ditto, but with ALL available info (eg debugging)
>  | Show_Internal Name		-- show the low-level representation.
>  | Show_Some Int			-- show x most recent entries
>  | Show_Something			-- show 'default_length' entries
>  | Show_MetaVars			-- metavars remaining to be solved
>  | Show_Everything		-- what it says

---

> showCtxt :: CtxtShow -> Ctxt -> String

---
showing single entries.

> showCtxt (Show_Entry n) c    
>  = longRender $ show_ctxt_entry Abbrev c n $$ text ""

> showCtxt (Show_Detailed n) c 
>  = longRender $ show_ctxt_entry Full c n $$ text ""

> showCtxt (Show_Internal n) c 
>  = case findCtxt n c of
>		Fail m -> "FAIL: " ++ m
>		Ok (CE (Def d _) ty)   -> "Def:\n" ++ finite_pcp d `sh_ty` ty
>		Ok (CE (FnDef a r _) ty)
>		                       -> "Fn:"++show a++"\n"++finite_pcp r `sh_ty` ty
>		Ok (CE cek ty)         -> finite (show cek) `sh_ty` ty
>    where
>		finite = take 20000 
>		finite_pcp = finite . pretty_crude_print
>		-- finite_fn  = finite_pcp . unpackReductionFn
>		sh_ty x ty = x ++ "\n" ++ finite_pcp ty ++ "\n\n"

---
Showing several entries.

> showCtxt (Show_Some i) c 
>  = "** Ctxt (" ++ show i ++ " most recent) **\n" 
>    ++ longRender (show_ctxt (take_relevant i) c $$ text "" $$ text "")

> showCtxt (Show_Everything) c 
>  = "** Ctxt **\n" ++ longRender (show_ctxt id c $$ text "" $$ text "") 

> showCtxt (Show_Something) c 
>  = showCtxt (Show_Some default_length) c

> showCtxt (Show_MetaVars) c
>  = "** Ctxt (MetaVars) **\n" 
>    ++ longRender (show_metavars c $$ text "" $$ text "") 

---

> default_length = 10 :: Int	-- the recent context

> take_relevant :: Int -> [[(Name,Ctxt_entry)]] -> [[(Name,Ctxt_entry)]]
> take_relevant n [] = []
> take_relevant n cess
>  = take n $ filter (\ces -> not $ null ces || (omit $ snd $ head ces)) cess
>    where
>		omit (CE (IntrosContinuation _ _) _) = True
>		omit _                               = False

---

> show_ctxt trim_f c@(Ctxt [])
>  = text "... is empty."

> show_ctxt trim_f c@(Ctxt es)
>  = vcat [ pp_no_def_type Abbrev (Ctxt ces) i ce 
>          | ((i,ce) : ces) <- reverse $ trim_f $ tails es ]

---
`show_ctxt_entry' is for showing individual entries.
  - entries are shown ONLY in the context they are in, 
  - binder vars won't get changed if the name appears LATER in the ctxt...

> show_ctxt_entry :: ShowDef -> Ctxt -> Name -> Doc
> show_ctxt_entry s c i 
>  = elimOk (\m -> text $ "Fail: " ++ m) 
>           (\(c',ce,_) -> pp_no_def_type s c' i ce) $
>    findNameInCtxt i c

---
`show_metavars'
  - select the MetaVar lines plus their dependencies.
  - also show Hyps - since these are the main things used to solve metavars
  - use show_ctxt to show the result, with trim_f as (const sub_ctxt)...

> show_metavars :: Ctxt -> Doc
> show_metavars c@(Ctxt es)
>  = show_ctxt (const $ tails ces) c
>    where
>		ces = catMaybes $ snd $ mapAccumL select [] es
>		select ns ce@(n, CE (M_V _) ty) = (namesIn ty ++ ns, Just ce)
>		select ns ce@(n, CE (Hyp _) ty) = (namesIn ty ++ ns, Just ce)
>		select ns ce@(n, CE _       ty) | n `elem` ns = (namesIn ty++ns,Just ce)
>		                                | otherwise   = (ns,            Nothing)


---
`showInCtxt' is intended for debugging, showing term plus relative ctxt.

> showInCtxt :: Ctxt -> Int -> Term -> String
> showInCtxt c n t 
>  = showCtxt (Show_Some n) c 
>		++ "\n" 
>		++ longRender (text "    =>" <+> showTerm c t)

---
`pp_no_def_type' does layout of single context entries.
  - ShowDef is flag for whether definitions are shown in full or whether they
    will be abbreviated if they are longer than `def_length_limit'

> data ShowDef = Abbrev | Full deriving Eq 
> def_length_limit = 50 :: Int

> pp_no_def_type :: ShowDef -> Ctxt -> Name -> Ctxt_entry -> Doc
> pp_no_def_type s c i (CE (Def rhs _) ty) 
>  = pp_any_def s c (text "Def" <+> stext i) rhs ty

> pp_no_def_type _ c i (CE (Hyp _) ty)
>  = sep [	text "Hyp" <+> stext i <+> colon, 
>			nest 4 $ showTerm c ty]

> pp_no_def_type _ c i (CE (M_V _) ty)
>  = sep [	text "M_V" <+> stext i <+> colon, 
>			nest 4 $ showTerm c ty]

> pp_no_def_type _ c i (CE (ElimOp a _) ty)
>  = sep [	text "E_O" <+> stext i <+> parens (stext a) <+> colon, 
>			nest 4 $ showTerm c ty]

> pp_no_def_type s c i (CE (FnDef a r _) ty)
>  = pp_any_def s c 
>				(text "CFn" <+> stext i <+> parens (text $ show a)) 
>				r
>				ty

> pp_no_def_type s c i (CE fam@IndFamily{} ty)
>  = sep [text "Ind" <+> stext i <+> parens (text cs_ns) <+> colon,
>			 nest 4 $ showTerm c ty]
>    where
>	cs_ns = unwords $ map show $ constrs fam

> pp_no_def_type s c i (CE co@(Coercion{}) ty)
>  = pp_any_def s c (text "Co " <+> stext i) (term co) ty

> pp_no_def_type s c i (CE (Dynamic gen) ty)
>  = text "Dyn" <+> (stext i <+> text "<<dynamic id generator>>"
>                    -- $$ showTerm c (fromOk $ gen (init $ show i) 1000)
>                    )

> pp_no_def_type _ c i (CE FileOpen ty)                
>  = text "***" <+> text " Open File" <+> stext i
> pp_no_def_type _ c i (CE FileClose ty)                
>  = text "***" <+> text "Close File" <+> stext i
> pp_no_def_type _ c i (CE FileCalled ty)                
>  = text "***" <+> text "Re-import File" <+> stext i


> pp_no_def_type _ c i (CE oth ty)                
>  = sep [	text "Oth" <+> stext i <+> equals, 
>			nest 4 $ stext oth,
>			nest 4 $ showTerm c ty]


---
`pp_any_def' -- for any definition; distinguished only by varying header.

> pp_any_def s c header rhs ty
>  = sep [	header,
>			nest 4 $ sep [	equals <+> def_txt, 
>							colon_if_not_kind ty $ showTerm c ty]]
>    where
>		pp_rhs = shortRender $ showTerm c rhs
>		short_enough = null $ drop def_length_limit pp_rhs
>		def_txt | s == Full    = showTerm c rhs
>		        | short_enough = showTerm c rhs
>		        | otherwise    = text "..."

