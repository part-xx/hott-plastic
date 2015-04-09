> module SharedSyntax where

Syntax components which are used in both concrete and abstract representations

> import Data.Char(isDigit)
> import Data.Maybe(fromMaybe, maybeToList)
> import Control.Monad(foldM, liftM, liftM2, liftM3)

> import BaseClasses(Permutable(..))
> import BaseTypes -- (Permutable(..))

#ifdef __HASKELL98__
#else
#define fmap map
#endif


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Common Synonyms.

> type Arity = Int

NB Name is for basic labels. 
   Ident is the version for Ctxt/Namespace access.

---

> type Label = Name		-- for record fields

allows Foo.(*)

---
Anything but call it a string! 

> type Ident = String     

---
Name 
  - represents how the (basic) name occurred.
  - fixity information will get attached later
  - ...


> data Name 
>  = Operator String 
>  | Plain Ident
>  | Backticks Ident
>  | CoName CoercionName
>  -- | UnivName Ident Int
>  deriving (Eq,Ord)

> instance Show Name where
>	showsPrec _ (Plain n)      = showString n
>	showsPrec _ (Operator o)   = showChar '(' . showString o . showChar ')'
>	showsPrec _ (Backticks n)  = showChar '`' . showString n . showChar '`'
>	showsPrec p (CoName n)     = showsPrec p n 
>    -- showsPrec p (UnivName n l) = showsString n . showChar univ_separator . shows l

---
CoercionName
 - represents how the coercion was built.
 - NB no distinction about direction of composition.
 - Final vsn - coercion name as sequence of coercions.
 - (this just for information...)

> data CoercionName
>  = CN [Ident]
>    deriving (Eq,Ord)

> instance Show CoercionName where
>	showsPrec _ (CN [])  = error "Empty coercion name"
>	showsPrec _ (CN [n]) = showString n
>	showsPrec _ (CN ns)  
>	 = in_pars $ foldr1 (\a b -> a . showChar ' ' . b) (map showString ns) 
>	   where
>		in_pars f = showChar '<' . f . showChar '>'


---

> data Fixity 
>  = Fixity Level Association 
>  | UnknownFixity
>    deriving (Eq,Show)

> data Association = AssocLeft | AssocNone | AssocRight deriving (Eq,Show)
> type Level = Int

Name replaces string in the syntax. 
 - but has the problem of double entries for the op vs plain occs? 
 - otherwise pay cost of lots of occs.
 - ideally, proper names get done in ParsingAux? (use HOF to simplify?)  
 - implies, Term is known to SimpleTerm (urgh!)

Forget the possibility of module-qualified-names 
 - use records to model this?
 - YES. 

---

However: records to ctxt then we have to store ctxt-like info in the rec? 

must also allow local defs ?
 - if we want abbreviations at top level. 
 - but can use manifest fields to do this - such fields need to be part of 
	the signature.

Implies, field name must be a Name not a plain String? 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%-------------------------------------------------------------------------------
Enforce the distinction between underscores (not-bound names) and bound names

> data BoundName = Underscore | Bound Name deriving Eq
> instance Show BoundName where
>	showsPrec _ Underscore = showString "_"
>	showsPrec p (Bound n)  = showsPrec p n

> unbind :: BoundName -> Name
> unbind (Bound n)  = n
> unbind Underscore = error $ "Unbind on Underscore"


%-------------------------------------------------------------------------------
HOW MUCH SHARING? 

> data Extension t
>  = SoftCast t t               -- normal cast.
>  | HardCast t t               -- cast assigns type, eg univ 

>  | Case [t] [([t], t)]		-- [scrutinees] [([pattern],result)]

>  | With t Name t (Maybe t)    -- l with r=a (:A) (NB typing optional)  
>  | Restrict t Name [Name]  	-- l|r - with list of intervening fields
>  | Project t Name [Name] (Maybe t)
>								-- l.1 - with intervening fields 
>								-- and (maybe) manifest term

>  -- for manifest records.
>  | Sig t Name (OpaqueOrManifest t)
>  | Rec t Name t t 
>  deriving Show

---
`OpaqueOrManifest'
  - signals whether entry is standard or manifest
  - plain is interpreted by ctxt, ie is Hyp in Sig, Def in Str.

> data OpaqueOrManifest t
>  = Opaque t
>  | Manifest t t		
> instance Show t => Show (OpaqueOrManifest t) where
>	showsPrec p (Opaque t)
>	 = showChar ':' . showChar ' ' . showsPrec p t
>	showsPrec p (Manifest t ty) 
>	 = showChar '='. showChar ' '. showsPrec p t. showChar ':'. showsPrec p ty

---

> instance Functor Extension where
>	fmap f (SoftCast t ty) = SoftCast (f t) (f ty)
>	fmap f (HardCast t ty) = HardCast (f t) (f ty)
>	fmap f (Case ts arms)  = Case (fmap f ts) 
>	                              [ (fmap f ps,f rhs) | (ps,rhs)<-arms ]

>	fmap f (Sig l n v)     = Sig (f l) n (fmap f v)
>	fmap f (Rec l n v ty)  = Rec (f l) n (f v) (f ty)

>	fmap f (Restrict t n ns)  = Restrict (f t) n ns
>	fmap f (Project t n ns a) = Project  (f t) n ns (fmap f a)
>	fmap f (With r n v t)     = With (f r) n (f v) (fmap f t) 

> instance Functor OpaqueOrManifest where
>	fmap f (Opaque t)      = Opaque (f t)
>	fmap f (Manifest t ty) = Manifest (f t) (f ty)


---
`fold_Extension'
 - fold a function over extension constrs
 - j is a join fn, f is the fn to fold.

> fold_Extension :: ([a] -> a) -> (t -> a) -> Extension t -> a
> fold_Extension j f (SoftCast t ty) = j $ fmap f [t,ty]
> fold_Extension j f (HardCast t ty) = j $ fmap f [t,ty]
> fold_Extension j f (With r n v t)  = j $ fmap f $ [r,v] ++ maybeToList t
> fold_Extension j f (Case ts arms)  
>  = j $ fmap f $ concat $ ts : [ ps ++ [a] | (ps,a) <- arms]

> fold_Extension j f (Project t n ns mt) = j $ fmap f $ t : maybeToList mt
> fold_Extension j f (Restrict t n ns)   = j [f t]
>		-- should ignore a? 
>		-- TO BE CHECKED.

> fold_Extension j f (Rec l _ v ty)            = j $ fmap f [l, v, ty]
> fold_Extension j f (Sig l _ (Opaque v))      = j $ fmap f [l, v]
> fold_Extension j f (Sig l _ (Manifest v ty)) = j $ fmap f [l, v, ty]



---
`mapM_Extension'
 - fold a monad function over extension constrs
 - j is a join fn, f is the fn to fold.

> mapM_Extension 
>  :: (Functor m, Monad m) => (a -> m b) -> Extension a -> m (Extension b) 

> mapM_Extension f (SoftCast t ty)   = liftM2 SoftCast (f t) (f ty)
> mapM_Extension f (HardCast t ty)   = liftM2 HardCast (f t) (f ty)
> mapM_Extension f (Project t n ns mt)  
>  = liftM2 (\t -> Project t n ns) (f t) (permute_map f mt) 
> mapM_Extension f (Restrict t n ns) 
>  = liftM (\t -> Restrict t n ns) (f t) 

> mapM_Extension f (With r n v t)  
>  = do { r' <- f r ; v' <- f v ; t' <- permute_map f t ; return (With r' n v' t') }

> mapM_Extension f (Case ts arms)  
>  = do 
>		ts' <- mapM f ts 
>		arms' <- mapM fold_arm arms
>		return (Case ts' arms')
>    where
>		fold_arm (ps,a) = do { ps' <- mapM f ps; a' <- f a ; return (ps',a') }

> mapM_Extension f (Rec l n v ty)
>  = do { l' <- f l ; v' <- f v ; ty' <- f ty ; return (Rec l' n v' ty') }
> mapM_Extension f (Sig l n (Opaque v)) 
>  = do { l' <- f l ; v' <- f v ; return (Sig l' n (Opaque v')) }
> mapM_Extension f (Sig l n (Manifest v ty)) 
>  = do { l' <- f l ; v' <- f v ; ty' <- f ty ; 
>         return (Sig l' n (Manifest v' ty')) }




---
JUNK. JUNK.
(Universe decode?) - no, this gets done at concrete level, since it is
	"in fwk". 

Can dot and hat be typed here? - not really, since the id is overloaded.

NB might want to collapse Project/Restrict - or am I trying to distinguish possibity
of no-label

%---------------------------------------
El as modifier too? - but maybe deserves special treatment as prim.
as Ctxt name - it isn't an applyable fn. Plus, special syntax in bdrs? 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

`BindVariety' 
  - indicates how binding was signified.
  - this is translated to binding by a specific constructor.

OPERATOR. 
By Constr - named directly
	(special case might be needed for explicit FO/DP?)

By Operator - HARD TO DETECT? in parser. 
	BUT this drops to a normal name? and can be used
	EXCEPT FOR "->", which we might allow as special case? - HAVE TO! 

By Brackets    
	=> parallel to the PP style? 
	how arbitrary? (hehe > < ... ! )
	BUT this is confusible with ">" (in Ord etc)
	looks like separate sets for symbols and bracket-symbols? (or overlap?)

CAN WE USE THIS IN PP? 

> data BindVariety
>  = By_Constr String           -- the main one. XX check on syntax?
>  | By_Brackets String String  -- brackets are paired.
>  | By_FO
>  | By_DP
>  deriving Show




%-------------------------------------------------------------------------------
MIGHT CHANGE? 

`univ_separator'
  - hack to identify universe symbols with Int/Id argument.
  - idea is that lexer recognises them as a unit, and passes them through 
    to parser as normal ids (but containing this symbol)
  - SimpleToTerm will do the actual decoding.
  - eg Type~2, Type~foo

  - NB in theory, universe names are LEXICAL CONSTANTS, as opposed to 
    application terms in the language.
  - It might be better to register universe hierarchy ids and detect that way?
    [ but lexer becomes context-depd... (but arrows do it too!) ]

Aug03: put as name constr, or (better) internalise the names...
 
> univ_separator :: Char
> univ_separator = '^'


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Building and Splitting Universe-related IDs.

> type UniverseLevel = Int
> type Mk_U a = Ident -> UniverseLevel -> a

---


> mk_uid :: Mk_U Name
> mk_uid p i = Plain $ p ++ univ_separator : show i

---

> split_UID :: Name -> (Ident, UniverseLevel)
> split_UID n 
>  = elimOk (\m -> error $ "split_UID (" ++ show n ++ "): " ++ m) id $ 
>    split_uid n

> split_uid :: Name -> OkF (Ident, UniverseLevel)
> split_uid (Plain i)
>  | null xs              = Fail $ "Not a universe id (" ++ i ++ ")"
>  | null ds              = Fail $ "Universe name had no suffix (" ++ i ++ ")"
>  | not $ all isDigit ds = Fail $ "Universe suffix not numeric (" ++ i ++ ")"
>  | otherwise            = Ok (cs, read ds)
>  where
>		(cs,xs) = break (==univ_separator) i
>		(_:ds) = xs

> split_uid n = Fail $ "split_uid - only for Plain names, " ++ show n


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
IdOrNum

represents place where can give an id or a number, 
  -- used eg, in selection of targets in tactics.
  (Maybe is used to indicate absence.)

> type IdOrNum    = Either Name Int


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Bracket and Arrow Information

 - indicates to the lexer which symbols are brackets.

 - also carries info on which arrows are non-dep version of which brackets

 - NB have to represent more, since this file defines monad P too.

 - XXXX redo

 - just the form we need here...

> type BracketPair  = (String,String)
> type BracketArrow = String

> type ArrowInfo = [(BracketArrow, BracketPair)]




