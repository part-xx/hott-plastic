> module Terms (
>			Binding(..), 
>			DB_Index(..),
>			Ident(..),
>			Name(..),
>			BoundName(..),
>			Arity(..),
>			Term(..),
>			Type(..),
>			TypedTerm(..),
>			Extension(..),

>			isFO, isDP, isBinder, isEl, isDef, isAtom, 
>			isCAST, isIVar,
>			is_kind_Type, is_kind_Prop,

>			binder_body,
>			nameFromTerm,
>			flattenAp,
>			unCAST,
>			cAST,

>			crude_print,
>			pretty_crude_print,
>			unknown_constructor,
>			found_Kind,

>			unflatten,
>			build_ap,
>			FlatTerm(..),

>			viewClosedReductionFn,
>			IotaReductionFn(..),
>			ClosedReductionFn(..)

>		) where

Type of terms used in calculation.

#ifdef __HASKELL98__
#define isAlphanum isAlphaNum
#endif

> import Data.Char(isAlphanum)
> import Base
> import SharedSyntax

> infixl 0 `j`

STRICTNESS ! ! (LATER!!!)

Plus Basic Term manipulations
	- maybe should split module...?
	- NO - since there can be NO context ops here
	- so just simple beta reduction (substit) on terms.
	- I think alpha may need context...

	- maybe should have an elim function for the term type? would make fn
		def easier? but would not be so clear perhaps...

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Types/Synonyms used in/with terms

> type DB_Index = Int

> type Type = Term
> type Binding = (BoundName, Type)

---

> type RecordLabel = String
> type ManifestRecord = () 				-- temporary!

---
A common abstraction.

> data TypedTerm = TT Term Type deriving Show


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Term structure

Non-strict terms are an exception. Default is to be strict.

#ifdef NONSTRICT_TERMS
# define S_Term Term
#else
# define S_Term !Term
#endif


%-------------------------------------------------------------------------------
IMPORTANT

Unsure whether I want the recursion in the type to be strict or not.
It seems to be worth about 10% for crude reduction test ("million").
But it could be counter-productive when the term is large.

NB should carefully check that strictness of term handling is EXACTLY as I
	think it is...

%-------------------------------------------------------------------------------


> data Term
>  = Type
> --  | Prop
>  | Kind				-- this is only for internal use (eg for definitions)

>  | Elim  !Name !Arity	-- 

>  | F_Elim !Name 			-- fast elim - in saturated (recursive) calls.

>  | Const   !Name  S_Term

>  | F_Const !Name !IotaReductionFn	-- faster form. 
>					-- a constant which has some elim rule.
>					-- term is fn, expecting args from elim.
>					-- arity is constructor arity, not include elim args

>  | Closed S_Term !ClosedReductionFn			
>					-- even faster form. 
>					-- contains original term, so we can revert...

>  | Var !Name			-- unclassified id - a temporary thing
>						-- should not occur in fully-translated terms.

>  | IVar     !DB_Index	-- de Bruijn index for (lambda-) bound vars.
>  | LocalDef !DB_Index S_Term
>						-- local def, with rhs (shifted appropriately)
>						-- treated as DeBruijn var; reduction can modify scope
>						-- replaced with def when binding let form is reduced
>						-- also contains shifted term

>  | GlobalDef  !Name S_Term
>						-- global def, together with its rhs for convenience.
>  | ContextVar !Name	-- assumption
>  | MetaVar    !Name	-- meta var.


--- 
Recursive bits

>  | El S_Term			-- 

>  | Ap S_Term S_Term

>  | DP BoundName S_Term S_Term	-- NB we only have 2 kinds of binder in LF.
>  | FO BoundName S_Term S_Term	-- user's binders are translated to these...

>  | Let !Name S_Term S_Term -- and a place-marker for Let constructs. 
>							-- this is only so user can see them...

>  | Co S_Term S_Term	-- application of coercion, is usually invisible 

---
Extensions 

>  | Ext ! (Extension Term)

---
THIS MAY BE NEEDED FOR FAST FILE READ.
<>	deriving (Read)

---
Re: STRICTNESS 
 - principle - all non-essential (computational...) parts are left non-strict
 - important things - Ap and FO bodies. 
 - so, type values in binders and casts won't be forced. 
 - maybe - second part of Ap can be left non-strict, since we go leftwards? 
 - what about DP body - might relax this.

%---------------------------------------
TESTING 

see if can use this to speed up reading

might need some adjustment for HO stuff, eg consts/closeds
	(ie, rewrite when context gets declared?)

IDEA: mark lines - to read-to-term, or parse-via-simple-term for cmds. 

> instance Read (a -> b) where
>	readsPrec _ s
>	 | fn_token == token = [(\x -> error "reading function", rest)]
>	 | otherwise = error "no read possible for fn"
>	 where
>		fn_token = "<<function>>"
>		(token,rest) = splitAt (length fn_token) s

<> t_r = crude_print . read :: String -> String

---
Showing terms

 - for deep debugging, might want to add "deriving Show" to the above.
 - but the following will do for virtually all cases.
 - (could add flag to crude_print on whether to show the infinite bits.)

> instance Show Term where
>		showsPrec 0 t = showString $ crude_print t
>		showsPrec _ t 
>		 | isAtom t  =           showString $ crude_print t
>		 | otherwise = bracket $ showString $ crude_print t
>		   where bracket s = showChar '(' . s . showChar ')'




---
Stop anyone using Eq on Terms
 - terms should only be compared with conversion

> instance Eq Term where
>	t1 == t2 = error "Cannot compare Terms by Haskell Eq - use conversion"


---
Crude forcing function.

> instance Forceable Term where
>		force t = if length (crude_print t) == 0
>					then error "null in force of term" 
>					else t


---
`unknown_constructor' - error fn for case of new constructor
	(NB ghc can detect missing cases in patterns)

> unknown_constructor m t 
>  = error $ "Unknown constr in " ++ m ++ " = " ++ take 100 (show t)

---
for cases when Kind found in terms

> found_Kind :: String -> a
> found_Kind fn
>  = error $ "Found Kind in Term, in function: " ++ fn




%-------------------------------------------------------------------------------
Simple predicates

> isAp, isEl, isFO, isAtom, isBinder :: Term -> Bool

> isAp (Ap _ _) = True
> isAp _        = False

> isFO (FO _ _ _) = True
> isFO _          = False

> isDP (DP _ _ _) = True
> isDP _          = False

> isBinder t = isFO t || isDP t

> isEl (El _) = True
> isEl _      = False

> isDef (GlobalDef _ _) = True
> isDef _               = False

---

> isAtom (ContextVar _)  = True
> isAtom (MetaVar _)     = True
> isAtom (IVar _)        = True
> isAtom (GlobalDef _ _) = True
> isAtom (Const _ _)     = True
> isAtom (F_Const _ _)   = True
> isAtom (Elim _ _)      = True
> isAtom (F_Elim _)      = True
> isAtom _               = False

---
`isEl_or_Type'
  - used (eg) for checking coercions.
  - NOT USED (jan 06)
   
<> isEl_or_Type :: Term -> Bool
<> isEl_or_Type Type   = True
<> isEl_or_Type (El _) = True
<> isEl_or_Type _      = False

---

> isCAST (Ext (HardCast _ _)) = True
> isCAST _                    = False

---

> isIVar (IVar _) = True
> isIVar _        = False	-- should test for LocalDef too? 


---
Test directly for Type
 - might want to extract the let-test, (see under_lets)

> is_kind_Type :: Term -> Bool
> is_kind_Type Type        = True
> is_kind_Type (Let _ _ t) = is_kind_Type t
> is_kind_Type _           = False

---
Test directly for Prop
 - might want to extract the let-test, (see under_lets)

> is_kind_Prop :: Term -> Bool
> is_kind_Prop (ContextVar (Plain "Prop")) = True
> is_kind_Prop (Let _ _ t)                 = is_kind_Prop t
> is_kind_Prop _                           = False



%-------------------------------------------------------------------------------
Simple Manipulations

> binder_body :: Term -> Term
> binder_body (FO _ _ t) = t
> binder_body (DP _ _ t) = t
> binder_body t          = error $ "binder_body on: " ++ take 200 (show t)

---
`flattenAp'
  - flatten n-ary applications to list of terms.

> {-# INLINE flattenAp #-}
> flattenAp :: Term -> (Term,[Term])
> flattenAp = flatten_ap [] 
> flatten_ap as (Ap l r) = flatten_ap (r:as) l
> flatten_ap as t        = (t, as)


---
`unCast'
  - remove the cast tag from a term, if there
  - if not a cast, then return unchanged.

> unCAST :: Term -> Term
> unCAST (Ext (HardCast e _)) = e
> unCAST t                    = t

---
Abbreviations
 
> cAST :: Term -> Term -> Term
> cAST e t = Ext (HardCast e t)
  




%-------------------------------------------------------------------------------

nameFromTerm

> nameFromTerm :: Term -> Maybe Name
> nameFromTerm (Type)          = Nothing
> nameFromTerm (Kind)          = Nothing
> -- nameFromTerm (Prop)          = Nothing

> nameFromTerm (Const i _)     = Just i
> nameFromTerm (F_Const i _)   = Just i
> nameFromTerm (Closed t _)    = nameFromTerm t
> nameFromTerm (Elim i _)      = Just i
> nameFromTerm (F_Elim i)      = Just i

> nameFromTerm (Var v)         = Just $ v
> nameFromTerm (IVar i)        = Just $ Plain $ "db_" ++ show i
> nameFromTerm (LocalDef i _)  = Just $ Plain $ "let_" ++ show i
> nameFromTerm (GlobalDef i _) = Just i
> nameFromTerm (ContextVar v)  = Just v
> nameFromTerm (MetaVar v)     = Just v

> nameFromTerm (El _)          = Nothing
> nameFromTerm (DP _ _ _)      = Nothing
> nameFromTerm (FO _ _ _)      = Nothing
> nameFromTerm (Ap _ _)        = Nothing
> nameFromTerm (Let _ _ e)     = Nothing

---
Debatable cases
 - since they are kind of wrappers, then maybe we should recurse on core term
 - decided to be conservative here; re-enable if needed.

> nameFromTerm (Co _ x)             = Nothing -- nameFromTerm x
> nameFromTerm (Ext (SoftCast e _)) = Nothing -- nameFromTerm e
> nameFromTerm (Ext (HardCast e _)) = Nothing -- nameFromTerm e

> nameFromTerm (Ext e)              = Nothing

<> nameFromTerm t = unknown_constructor "nameFromTerm" t



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
`crude_print' - used for showing terms in reasonable space

> crude_print, pretty_crude_print :: Term -> String
> crude_print        = c_p ""
> pretty_crude_print = c_p " " 

> add_sp "" = ""
> add_sp s  = s ++ "  "

> add_cs "" cs = ""
> add_cs s  cs = s ++ cs

> j [] b  = b
> j a  [] = a
> j a b   | in_id (last a) && in_id (head b) = a ++ " " ++ b
>         | otherwise                              = a ++ b
>         where
>				in_id c = isAlphanum c || c == '_' || c == '?' || c == '$'

> sep "" s = s				-- sep not needed if \n
> sep i  s = "" 

> indent "" s = s			-- switchable indent.
> indent i  s = "\n" ++ i ++ s

> without_indent s cs 
>  | null s    = cs
>  | otherwise = ""


> c_p s Type            = indent s "Type"
> -- c_p s Prop            = indent s "Prop"
> c_p s Kind            = indent s "Kind"

> c_p s (Const i _)     = indent s $ show i 
> c_p s (F_Const i _)   = indent s $ show i ++ "_F"

> c_p s (Closed t f)
>  = indent s $ "CT"
#if 1
>		++ "{{" ++ c_p (s++"  ") which_def ++ "}}\n" ++ s
>    where 
>		-- which_def = t 
>		which_def = unpack_rf f
#endif

> c_p s (Elim i a)      = indent s $ show i ++ "(" ++ show a ++ ")"
> c_p s (F_Elim i)      = indent s $ show i ++ "_F(sat)"

> c_p s (GlobalDef i _) = indent s $ "@" ++ show i 
> c_p s (ContextVar i)  = indent s $ show i 
> c_p s (MetaVar i)     = indent s $ "?" ++ show i 
> c_p s (Var i)         = indent s $ "$" ++ show i 
> c_p s (IVar i)        = indent s $ "v" ++ show i 
#if 0 
> c_p s (LocalDef i d)  = indent s $ "l" ++ show i 
#else 
> c_p s (LocalDef i d)  = indent s $ "l" ++ show i ++ "{" ++ show d ++ "}"
#endif

> c_p s (Co l r) 
>     = without_indent s "(Co"
>       `j` c_p (add_cs s " (") l
>		`j` without_indent s " "
>       `j` c_p (add_cs s " |") r
>       `j` without_indent s ")"

> c_p s (DP v ty t) 
>  = indent s ("{DP " ++ show v ++ " : ") 
>	 `j` c_p (add_sp s) ty ++ sep s " / " 
>	 `j` c_p (add_sp s) t 
>	 `j` indent s "}"

> c_p s (FO v ty t) 
>  = indent s ("[FO " ++ show v ++ " : ") 
>	 `j` c_p (add_sp s) ty ++ sep s " / " 
>	 `j` c_p (add_sp s) t 
>	 `j` indent s "]"

> c_p s (Ext (SoftCast e t))
>  = indent s "(" 
>    `j` c_p    (add_sp s) e 
>    `j` indent (add_sp s) "::"
>    `j` c_p    (add_sp s) t 
>    `j` indent s ")"

> c_p s (Ext (HardCast e t))
>  = indent s "(" 
>    `j` c_p    (add_sp s) e
>    `j` indent (add_sp s) ":!:"
>    `j` c_p    (add_sp s) t 
>    `j` indent s ")"

---

> c_p s (Let v rhs t) 
>  = indent s ("(Let " ++ show v ++ " = ")
>    `j` c_p (add_sp s) rhs 
>    `j` "in "
>    `j` c_p (add_sp s) t 
>    `j` indent s ")"

> c_p s (El t)
>  = indent s "El" 
>    `j` el_space (c_p (add_sp s) t) 
>    where
>		el_space s@('(':_) = s  
>		el_space s         = " " ++ s

> c_p s ap@(Ap l r) 
>  | null $ drop 1 args
>     = without_indent s "("
>       `j` c_p (add_cs s " (") l
>		`j` without_indent s " "
>       `j` c_p (add_cs s " |") r
>       `j` without_indent s ")"
>  | otherwise
>     = without_indent s "("
>       `j` c_p (add_cs s " (") hd 
>		`j` foldr1 j (map (c_p in_arg) args)
>       `j` without_indent s ")"
>  where
>		(hd,args) = flattenAp ap
>		in_arg = add_sp $ add_cs s " |"

---

> c_p s (Ext (Sig l n (Opaque v)))
>  = indent s ("(Sig:opaque " ++ show n)
>    `j` c_p (add_sp s) l
>    `j` c_p (add_sp s) v
>    `j` indent s ")"
> c_p s (Ext (Sig l n (Manifest v ty)))
>  = indent s ("(Sig:manifest " ++ show n)
>    `j` c_p (add_sp s) l
>    `j` c_p (add_sp s) v
>    `j` c_p (add_sp s) ty
>    `j` indent s ")"
>		
> c_p s (Ext (Rec l n v ty))
>  = indent s ("(Str: " ++ show n)
>    `j` c_p (add_sp s) l
>    `j` c_p (add_sp s) v
>    `j` c_p (add_sp s) ty
>    `j` indent s ")"

> c_p s (Ext (With l n v _))
>  = indent s ("(With: " ++ show n)
>    `j` c_p (add_sp s) l
>    `j` c_p (add_sp s) v
>    `j` indent s ")"

> c_p s (Ext (Project l n ns mt))
>  = indent s ("(Dot: " ++ show (n,ns) ++ "=" ++ show mt)
>    `j` c_p (add_sp s) l
>    `j` indent s ")"

> c_p s (Ext (Restrict l n ns))
>  = indent s ("(Bar: " ++ show (n,ns))
>    `j` c_p (add_sp s) l
>    `j` indent s ")"



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
FlatTerm
 - reduce exprs (by just beta-reduction) to form of 
	- an applic of var/const ie (C a1...an), or
	- an abstraction 
 - E-form is convenient for testing for other redexes
 - head const is strict, since it is often consulted. 
 - but the args aren't always needed, so left non-strict

> data FlatTerm
>  = E !Term [Term]
>  | A Term


> instance Show FlatTerm where
>	showsPrec 0 (A t) 
>	 = showString "A " .showString (crude_print t)
>	showsPrec n (A t) 
>	 = showString "(A " .showString (crude_print t) .showString ")"
>	showsPrec n (E t ts) 
>	 = showString "E " .
>		showString (unwords $ map crude_print $ t : ts)


---
`unflatten' rebuilds a FlatTerm.

> unflatten :: FlatTerm -> Term
> unflatten (A t)    = t
> unflatten (E a as) = build_ap a as

---
`build_ap' - I want to see how often this is done, hence the _scc_

> build_ap :: Term -> [Term] -> Term
> build_ap = foldl Ap

<> build_ap = _scc_ "build_ap" foldl Ap


%-------------------------------------------------------------------------------
IotaReductionFn
 - takes list of relevant terms from the match

> type IotaReductionFn = [Term] -> FlatTerm 


---
ClosedReductionFn
 - used for Closed FO-terms
 - compilation technique will compensate for term being in eta-long form or not
 - only allow printable form to be generated

> type ClosedReductionFn = [Term] -> FlatTerm 

> viewClosedReductionFn :: ClosedReductionFn -> String
> viewClosedReductionFn f = show $ unpack_rf f 


> unpack_rf f
>  = unflatten $ f (map (ContextVar . Plain . ("_!_"++)) $ reverse vars)
>    where
>		vars = map show [0..999]

['a'..'z'] ++ ['A'..'Z']



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#if 0
TESTING

> [va,vb,vc,vd] = map (Var.(:[])) "abcd"

> t1 = (Ap (Ap va vb) vc)
> t2 = FO "_" t1 (Ap (IVar 3) t1)

> p1 = putStr . crude_print
> p2 = putStr . pretty_crude_print

#endif
