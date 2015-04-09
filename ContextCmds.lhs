> module ContextCmds where

Top-level SMALL Context Operations

(see specific modules for others, eg tactics and comp rules)

> import Data.List (mapAccumL, sortBy, isPrefixOf, partition)
> import Control.Monad (foldM)
> import Data.Maybe

> import Base
> import Terms
> import TermOps(replaceTerminal, namesIn)
> -- import TermReduction(compileArbitraryDefTerm)
> import Context
> import ContextOps

> import CommandTypes
> import Printing
> import PrettyAux(longRender, shortRender, stext)
> import Pretty(text, char, nest, fsep, ($$), (<>), (<+>))
> import Pretty(vcat, lbrack, rbrack, empty)

> import Debugging (show_term)

> import GlobalOptions(p_Normal)

> import SimpleTerm
> import SharedSyntax(BindVariety(..))
> import SimpleToTerm(translate_db)

> import Reduction(dwhnf, hnf, ihnf)

> import TypeInference
> import Convertibility (convertible)

> import MetaVars (simple_MetaVars, definition_MetaVars)



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Faster lookups

> rebuildLookupTable :: IM
> rebuildLookupTable 
>  = do
>	p_Normal $ "Updating context lookup table"
>	c <- get_M_
>	set_M_ (fix_table c)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
File Handling

> openFileInCtxt  :: FilePath -> Ctxt -> Ctxt
> closeFileInCtxt :: FilePath -> Ctxt -> Ctxt
> fileDepInCtxt   :: FilePath -> Ctxt -> Ctxt
> openFileInCtxt  = add_file FileOpen
> closeFileInCtxt = add_file FileClose
> fileDepInCtxt   = add_file FileCalled


> filesInCtxt :: Ctxt -> [FilePath]
> filesInCtxt (Ctxt es)
>  = [ n | (Plain n, CE FileClose _) <- es ]

---

> add_file k n (Ctxt es) 
>  = Ctxt $ (Plain n, CE k $ ce_dummy_type "add file") : es

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Simplify a type.

> whnfOnType :: Name -> IM
> whnfOnType n 
>  = do
>		p_Normal $ "DWNHF on type of " ++ show n
>
>		c <- get_M_
>		(CE kind ty) <- embed $ findCtxt n c
>		-- assert_is_hyp_or_mv kind "whnfTheType"
>
>		let ty' = ihnf c (dwhnf ty)				-- ie, "ehnf"
>		c' <- embed $ chgCtxt n (\_ -> Ok $ CE kind ty') c
>		set_M_ c'

---

> expand :: CtxtReference -> Name -> IM
> expand r n
>  = do
>		ce <- use_M_ $ findCtxt n
>		assert_p (is_def ce) "Expand expects a definition."
>		tgt <- use_M_ $ getReference r
>		p_Normal $ "Expanding def " ++ show n ++ " in type of " ++ show tgt
>		chg_M_ $ chgCtxt tgt chg_ty
>    where
>		exp_n (GlobalDef m d) | n == m = Just d
>		exp_n _                        = Nothing
>		chg_ty (CE kind ty) = Ok $ CE kind $ replaceTerminal exp_n ty



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
CUT 

> cut :: Cut_Expression -> IM
> cut ce@(Cut_Expression n t)
>  = do
>		p_Normal $ "Cutting " ++ show n ++ " for " ++ show t
>		chg_M_ (internal_cut n (\nc -> translate_db nc t))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

> lift_over :: Name -> Name -> IM
> lift_over def tgt
>  = do
>		ctxt <- get_M_
>		ce <- embed $ findCtxt def ctxt
>		case ce of 
>			CE (Def _ _) _ 
>			  -> do
>					new_ctxt <- embed (lift_over_mv def ce tgt ctxt) 
>					set_M_ new_ctxt
>			_ -> fail_with_msg "lift_dependence can't handle this case."

> lift_over_mv :: Name -> Ctxt_entry -> Name -> Ctxt -> OkF Ctxt
> lift_over_mv nm (CE (Def def_rhs _) def_ty) tgt (Ctxt es)
>  = do
>		let ctxt = Ctxt $ filter (\(n,_) -> n /= nm) es
>		(pre_ctxt,_,_) <- findNameInCtxt tgt ctxt
>		inferType pre_ctxt def_ty
>		c1 <- insertBefore tgt (Ctxt [dummy]) ctxt
>		c2 <- internal_cut nm (\_ -> Ok def_rhs) c1
>		return c2
>    where
>		dummy = (nm, CE (M_V $ error "lift dummy") def_ty)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
`add_defs_decls' 
  - loops through list, rejecting the lot if any error occurred.

> add_defs_decls :: [Def_Decl] -> IM
> add_defs_decls ds 
>  = mapM_ add_def_decl ds
>    where
>		add_def_decl (Def_Decl (Right d)) = add_decl d 
>		add_def_decl (Def_Decl (Left d))  = add_def  d


---
`add_decl' does just that. 

> add_decl :: Declaration -> IM
> add_decl (Declaration ns ty)
>  = add_decl_or_mv (\n -> Hyp (ContextVar n)) ns ty


---
`claim' - like add_decl, but labels as a metavar.

> claim :: Maybe Name -> SimpleType -> IM
> claim Nothing ty 
>  = do
>	ns <- use_M_ $ \c -> Ok $ freeNamesOf c first_numbered_metavar
>	assert_p (not $ null ns) "Empty possibilities for new mv name"
>	claim (Just $ head ns) ty

> claim (Just n) ty 
>  = add_decl_or_mv (\n -> M_V (MetaVar n)) [n] ty

---
Group for faking simple proof mode.

> goal_marker = Plain "KillRef marker" 

> fake_goal :: Maybe Name -> SimpleType -> IM
> fake_goal opt_id t
>  = do
>	ok_find <- raiseOkF_M_ $ use_M_ $ findCtxt goal_marker
>	assert_p (isFail ok_find) 
>		"Can't start new goal - already one in progress"
>	add_if_unused goal_marker (CE Mark $ ce_dummy_type "fake_goal")
>	claim opt_id t

> killref :: IM
> killref
>  = do
>	ok_find <- raiseOkF_M_ $ use_M_ $ findCtxt goal_marker
>	assert_p (isOk ok_find) 
>		"Can't kill a proof which isn't in progress"
>	forget [goal_marker]
>	p_Normal "Forgotten back to previous goal"

> fake_save_proof :: IM
> fake_save_proof 
>  = do
>	ok_find <- raiseOkF_M_ $ use_M_ $ findCtxt goal_marker
>	assert_p (isOk ok_find) 
>		"Can't 'save' - there isn't a goal proof in progress"
>	c <- get_M_
>	let (Ctxt es) = c
>	let (pre,post) = break (\ce -> fst ce == goal_marker) es
>	assert_p (not $ any is_hyp_or_metavar [ cek | CE cek _ <- map snd pre ])
>		$ "can't have open subgoals or hypotheses inside proof"
>	assert_p (not $ null post) 
>		$ error "anomaly in save_proof" -- cautious - we just checked
>	set_M_ (Ctxt $ pre ++ drop 1 post)
>	p_Normal "Proof saved, and marker removed"


---
`add_decl_or_mv' 
  - checks WF of type, and that it is a kind, then adds it if names free.

  - might need El or Coercion insertion, hence we use the type returned by 
	inferKind.

> add_decl_or_mv :: (Name -> Ctxt_entry_kind) -> [Name] -> SimpleType -> IM
> add_decl_or_mv cek_f ns ty
>  = do
>		let free_names c = filter (`notElem` ns) . freeNamesOf c 
>		(_, ty1) <- get_M_ >>= \c -> simple_MetaVars (free_names c) ty
>		(TT ty2 ty_ty1) <- use_M_ (\c -> inferKind c ty1)
>		mapM_ (\i -> add_if_unused i (CE (cek_f i) ty2)) ns

<>		let update_ce c i = chgCtxt i (\ce -> Ok $ CE (cek_f i) ty2) c
<>		mapM_ (\i -> add_if_unused i (CE Mark $ ce_dummy_type "add_d/mv")) ns
<>		chg_M_ $ \c -> foldM update_ce c ns


---
`add_local_defs'
  - like adding defs, but inserts BEFORE the most recent metavar.
  - bit hacky? ie, with MC dependencies, this op is redundant! 

> add_local_defs :: [Definition] -> IM
> add_local_defs ds
>  = mapM_ add_local_defs_ [ Definition [n] pbs rhs mty 
>                           | Definition ns pbs rhs mty <- ds, n <- ns ]

> add_local_defs_ d
>  = do
>		[nce@(n,_)] <- add_def_ d
>		ok_find <- raiseOkF_M_ $ use_M_ $ findCtxt n 
>		case ok_find of
>			Ok _   -> fail_with_msg $ "Name (" ++ show n ++ ") already used."
>			Fail _ -> do
>						mv_nm <- use_M_ $ getReference Nothing
>						chg_M_ $ insertBefore mv_nm (Ctxt [nce])

---
`add_def'
  - add a definition to the ctxt if the name is unused.
  - the user optionally supplies a type.
  - checks that the rhs is well typed, and is convertible with the supplied
	type if applicable.

  - rhs can also contain metavars and be under-saturated wrt the type if
	supplied (in which case, metavars are added to achieve saturation).

> add_def :: Definition -> IM
> add_def d
>  = add_def_ d >>= \nces -> mapM_ (uncurry add_if_unused) nces

separate, so local def add can use it.

> add_def_ :: Definition -> CM [(Name, Ctxt_entry)]
> add_def_ (Definition ns [] rhs mty)
>  = do 
>		c_init <- get_M_

>		(rhs',ty') <- definition_MetaVars (freeNamesOf c_init) rhs mty
>				-- MAYBE too much work done in definition_MetaVars ? 

>		-- use_M_ (\c -> Ok $ "DEF: " ++ show_term c rhs' ++ "\nTYP: " ++ maybe "NOTHING" (show_term c) ty' ++ "\n") >>= print_M_ 

>		(TT rhs'' ty'')
>			 <- catch_M_ problems_with_type $
>				case ty' of
>					Nothing  -> use_M_ (\c -> inferType c rhs')
>					Just ty2 -> use_M_ (\c -> typeCheckRawKind c rhs' ty2)

>		let ct = rhs'' 
>		-- let ct = compileArbitraryDefTerm rhs'' 	-- turned off.
>		return [ (n, CE (Def rhs'' $ GlobalDef n ct) ty'') | n <- ns ]
>    where
>		problems_with_type m
>		 = fail_with_msg $ "Couldn't get type for rhs: " ++ m

---
Here, there are some pre-binders, so we apply them to the rhs. 

The optional rhs type does not cover the pre-binders. We take the easy way out
and Cast the rhs to that type (if there) before applying the pre-binders. 

The hard way would be to collect the types from the pre-binders, and build the
new type from that. (QQ: How hard would it be?) 

> add_def_ (Definition n pbs rhs mty)
>  | isNothing mty
>  = add_def_ $ Definition n [] bound_rhs Nothing
>  | otherwise
>  = add_def_ $ Definition n [] bound_rhs (fmap (add_bdrs By_DP) mty)
>    where
>		add_bdrs by t = foldr (mkBinding by) t pbs
>		bound_rhs = add_bdrs By_FO rhs



%-------------------------------------------------------------------------------
`add_if_unused' enshrines principle that global names MUST be UNIQUE.

> add_if_unused :: Name -> Ctxt_entry -> IM 
> add_if_unused i ce
>  = do
>		c' <- use_M_ $ \c -> addCtxt (Bound i) ce c
>		
>		-- then fail_with_msg (show i ++ " -- NOT added - already in use")
>		do
>			set_M_ c'
>			p_Normal (showCtxt (Show_Entry i) c')


%-------------------------------------------------------------------------------
warn that concise type might be lost when coercion insertion occurs.

> warn_about_losing_type s 
>  = print_M_ $ "Warning - can't preserve type in " ++ s ++ "\n" 



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Forget and Discharge

NB these could be done as context splitting and loss? 

> forget :: [Name] -> IM
> forget []
>  = fail_with_msg "Forget expects some names"
> forget ns 
>  = change_msg (\s -> "Abandoning Forget command: " ++ s) $
>    do
>		c <- get_M_
>		ns_cs <- embed $ mapM (forget_ c) ns
>		let (ns2, c2) = last (sortBy cmp_by_length ns_cs)
>		p_Normal $ longRender 
>		                   (text "Forgetting " $$ nest 3 (fsep $ map stext ns2))
>		set_M_ c2
>    where
>		cmp_by_length a b = length (fst a) `compare` length (fst b)

---
forget_ 
  - looks for name (else fails)
  - returns new ctxt and forgotten names

> forget_ :: Ctxt -> Name -> OkF ([Name], Ctxt)
> forget_ c n
>  = do
>		ok <- findCtxt n c		-- n must be there. WHY? XXXX 
>		let (Ctxt es) = c
>		let ns = takeWhile (/=n) $ map fst es
>		return (ns ++ [n], Ctxt $ drop (length ns + 1) es)



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Dumping context to individual modules (quick hack)

> dump_modules c@(Ctxt es) 
>  = do
>	let (to_dump, to_import) = partition dumpable $ fragment init_stack 
>	                         $ reverse es
>	let imports = [ n | ModuleStuff n _ <- to_import ]
>	mapM_ (process_module c) to_dump
>	let entries = concat [ ces | ModuleStuff _ ces <- to_dump ]
>	writeFile "a/All.o" 
>	  $ unlines [ "> " ++ shortRender (dumpCE c ce) ++ ";"
>	            | ce <- entries, keep_file_op imports ce ]
>    where
>	keep_file_op imports (_, CE FileOpen _)   = False
>	keep_file_op imports (_, CE FileClose _)  = False
>	keep_file_op imports (n, CE FileCalled _) = n `elem` imports
>	keep_file_op imports _                    = True

---

> dumpable (ModuleStuff fn ces)
>  | base_name `elem` ["Sol", "TypeN"]
>  = False
>  | has_nonstandard_elim 
>  = False
>  | otherwise
>  = True
>    where
>	base_name = simplify_nm fn
>	has_nonstandard_elim 
>	 = not $ null [ i | (_, CE i@IndFamily{} _) <- ces
>	                  , "non-standard" `isPrefixOf` input i ]

---


> process_module c (ModuleStuff fn ces)
>  = do
>	let fn' = simplify_nm fn
>	putStrLn $ "Dumping module:" ++ show fn'
>	writeFile ("a/" ++ fn' ++ ".o") 
>	          $ unlines [ "> " ++ shortRender (dumpCE c ce) ++ ";" 
>	                    | ce <- ces ]


> simplify_nm :: Name -> String
> simplify_nm 
>  = reverse . takeWhile (/='/') . tail . dropWhile (/='.') . reverse . show


---

> data ModuleStuff a = ModuleStuff Name [a]
> init_stack = [(Plain "Toplevel.lf", [])]

> -- fragment :: [[a]] -> [a] -> [ModuleStuff a]
> fragment [(fn,ls)] []
>  = [ ModuleStuff fn ls ]		-- should only be one left.

> fragment s ((fn, CE FileOpen _):ces)
>  = fragment ((fn, []) : s) ces

> fragment ((fn, ls):(fn2, ls2):s) ((_, CE FileClose _):ces)
>  = ModuleStuff fn ls : fragment ((fn2, ls2 ++ [load_line]):s) ces
>    where
>	load_line = (fn, CE FileCalled Kind) 

> fragment ((fn, ls):s) (nce : ces)
>  = fragment ((fn, ls ++ [nce]):s) ces


---


> dumpCE :: Ctxt -> ID_CE -> Doc

> dumpCE c (fn, CE FileCalled _)
>  = text "import" <+> text (simplify_nm fn)

> dumpCE c (n, CE d@Def{} ty)
>  = (lbrack <> stext n <+> char '=' <+> showTerm c (rhs d) <> rbrack)

> dumpCE c (n, CE Hyp{} _)
>  = text "ECHO what do do with hyp" 

> dumpCE c(n, CE M_V{} _)
>  = text "ECHO what do do with m_v" 

> dumpCE c (n, CE i@IndFamily{} ty)
>  = text (input i)			-- as saved earlier(!)

---
Coercion case
 - just treat as defn, since we don't want the overhead of search

> dumpCE c (n, CE co@Coercion{} ty)
>  | co_rule co
>     = error "can't handle co rules yet"	-- can't easily identify prereq
>  | otherwise
>     = dumpCE c (n, CE (Def {rhs = term co}) ty)
>						-- treat as def (see above)

IDEA:
 I'm using eta-long form, using lambda to fill in metavars
 Of course, could remove the etas and put the (FRESH) metavars in directly
 BUT: this loses clarity about which coercion used? 

---



> dumpCE c (n, CE Table{} _)
>  = empty

> dumpCE c (n, CE ElimOp{} ty)
>  = empty

> dumpCE c (n, CE FnDef{} ty)
>  = empty

> dumpCE c (n, CE cek ty)
>  = text $ "ECHO don't know what to do for " ++ show n


%---------------------------------------

