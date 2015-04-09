> module SimpleTerm (module SimpleTerm, Name, Arity, BoundName) where

The term structure used for parsing. 
Simplified so as to be useful for Specified TT's too.

> import Data.Char(isDigit)
> import Data.Maybe(fromMaybe)
> import Control.Monad(liftM, liftM2, liftM3)
> import Base(OkF(..),Permutable(..), elimOk, join_words)
> import SharedSyntax

> type SimpleType = SimpleTerm

%-------------------------------------------------------------------------------
SimpleTerm
 - the Concrete syntax
 - this language is intentionally very basic
 - it will be used for LF and for object theories.

 - Ext_S contains non-std extensions. 

> data SimpleTerm
>  = Name_S Name				-- Any kind of std name, eg x, _, ?n, ?x

>  | Ap_S SimpleTerm SimpleTerm

>  | Op_S SimpleTerm SimpleTerm SimpleTerm	-- use of binary op., middle is op

>  | Let_S Name SimpleTerm SimpleTerm		-- is a binder. (NB no _ allowed)

>  | Bind_S BindVariety BoundName SimpleType SimpleTerm

>  | Pars_S SimpleTerm                      -- marker for precedence.

>  | Ext_S (Extension SimpleTerm)           -- for other constructs, eg cast



%-------------------
Aux fns.

> mkBinding k (vs,ty) t = foldr (\v -> Bind_S k v ty) t vs

---

> mk_Name_S :: Name -> SimpleTerm
> mk_Name_S = Name_S

> underscore_name = Name_S $ Plain "_" 




%-------------------------------------------------------------------------------
`showSimpleTerm' 
  - a simple show function for SimpleTerms.
  - this is used as overloaded show.
  - NB can be faster (useful? maybe) if use showString IN showSimpleTerm

> instance Show SimpleTerm where
>	showsPrec _ = showString . showSimpleTerm

---

> showSimpleTerm :: SimpleTerm -> String
> showSimpleTerm t@(Name_S i)  = show i

> showSimpleTerm (Ap_S t1 t2)  
>  = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

> showSimpleTerm (Op_S t1 op t2)  
>  = "(" ++ show t1 ++ " " ++ show op ++ " " ++ show t2 ++ ")"

> showSimpleTerm (Bind_S v m ty t)
>  = join_words (show v)
>    ++ "[" ++ show m ++ ":" ++ show ty ++ "]" ++ show t

> showSimpleTerm (Let_S m rhs t)
>  = "[" ++ show m ++ "=" ++ show rhs ++ "]" ++ show t

> showSimpleTerm (Pars_S t) = "(" ++ showSimpleTerm t ++ ")"

> showSimpleTerm (Ext_S (Rec l n v ty)) 
>  = "str(" ++ show l ++ ", " ++ show n 
>			++ " = " ++ show v ++ " : "	++ show ty ++ ")"
> showSimpleTerm (Ext_S (Sig l n (Opaque v))) 
>  = "str(" ++ show l ++ ", " ++ show n ++ " : " ++ show v ++ ")"
> showSimpleTerm (Ext_S (Sig l n (Manifest v ty)))
>  = "str(" ++ show l ++ ", " ++ show n 
>     ++ " = " ++ show v ++ " : " ++ show ty ++ ")"

> showSimpleTerm (Ext_S (With l n v _))
>  = show l ++ " with " ++ show n ++ " = " ++ show v

> showSimpleTerm (Ext_S e)
>  = "NEW_EXT(" ++ show e ++ ")"



---
Simpler version, sometimes needed.

> showNicely (Bind_S By_DP n dom rng)
>  = "(" ++ show n ++ ":" ++ showNicely dom ++ ")" ++ showNicely rng
> showNicely (Bind_S By_FO n dom body)
>  = "[" ++ show n ++ ":" ++ showNicely dom ++ "]" ++ showNicely body
> showNicely (Name_S n) = show n
> showNicely (Ap_S t1 t2) = "(" ++ showNicely t1 ++ " " ++ showNicely t2 ++ ")"
> showNicely (Pars_S t) = "(" ++ showNicely t ++ ")"



%-------------------------------------------------------------------------------
`occursIn' 
  - checks if name occurs FREE in a term.
`occursIn_p'
  - generalised vsn, to suit metavar detection. 

> occursIn :: Name -> SimpleTerm -> Bool
> occursIn n = occursIn_p (==n)

> occursIn_p :: (Name -> Bool) -> SimpleTerm -> Bool

> occursIn_p p (Name_S i) = p i
> occursIn_p p (Ap_S t1 t2) = occursIn_p p t1 || occursIn_p p t2

> occursIn_p p (Pars_S t) = occursIn_p p t

> occursIn_p p (Bind_S _ Underscore ty t)     -- name not here
>  = occursIn_p p ty || occursIn_p p t
> occursIn_p p (Bind_S _ (Bound m) ty t)      -- 
>  | p m       = False
>  | otherwise = occursIn_p p ty || occursIn_p p t

> occursIn_p p (Let_S m rhs t)
>  | p m       = False
>  | otherwise = occursIn_p p rhs || occursIn_p p t

> occursIn_p p t
>  = error $ "occursIn_p unknown constructor: " ++ show t


%-------------------------------------------------------------------------------


> replaceName_S :: Name -> SimpleTerm -> SimpleTerm -> SimpleTerm
> replaceName_S n t = replace_ (replace_Name n t)

> replace_Name n t (Name_S m) | m == n = t
> replace_Name _ _ o                   = o

> replace_ :: (SimpleTerm -> SimpleTerm) -> SimpleTerm -> SimpleTerm
> replace_ rf t@(Name_S _)      = rf t
> replace_ rf (Ap_S t1 t2)      = Ap_S (replace_ rf t1) (replace_ rf t2)
> replace_ rf (Bind_S v m ty t) = (Bind_S v m (replace_ rf ty) (replace_ rf t))
> replace_ rf (Let_S m rhs t)   = (Let_S m (replace_ rf rhs) (replace_ rf t))
> replace_ rf (Pars_S t)        = Pars_S $ replace_ rf t
> replace_ rf (Ext_S e)         = Ext_S $ fmap (replace_ rf) e


%-------------------------------------------------------------------------------
`CompRuleFunction' is the basic specification of a function to be added as a
comp rule.

> data CompRuleFunction 
>  = CRF 
>		Name        -- of the constructor.
>		Arity 		-- arity of constr/constant
>		SimpleTerm 	-- the whole function
>		SimpleType	-- the type of this function.
>    deriving Show


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Fns for manipulating binders.

---
`ap_nms' - construct an expression from the applic of name n to names ns.
`ap_ts' - does same but for SimpleTerms throughout.

> ap_nms :: Name -> [Name] -> SimpleTerm
> ap_nms n ns = foldl Ap_S (Name_S n) $ map Name_S ns

> ap_ts :: SimpleTerm -> [SimpleTerm] -> SimpleTerm
> ap_ts = foldl Ap_S 

---

DEPRECATED - prefer to use surgery on terms? 

> type DP_Binding = (BoundName, SimpleType)

> un_bs_FO :: [DP_Binding] -> SimpleType -> SimpleType
> un_bs_FO bs e = foldr (\(n,ty) t -> Bind_S By_FO n ty t) e bs

> un_bs :: [DP_Binding] -> SimpleType -> SimpleType
> un_bs bs e = foldr (\(n,ty) t -> Bind_S By_DP n ty t) e bs



---
`decomposeTmpCmd'
  - converts an applic of an id to a series of simple terms into
    a flattened version, or Fail.
 
> decomposeTmpCmd :: SimpleTerm -> OkF (Name, [SimpleTerm])
> decomposeTmpCmd = decompose_tmp_cmd_ []
> decompose_tmp_cmd_ rr (Ap_S l r) = decompose_tmp_cmd_ (r:rr) l
> decompose_tmp_cmd_ rr (Name_S n) = return (n, rr)
> decompose_tmp_cmd_ _  t          = Fail $ "Couldn't decompose: " ++ show t



