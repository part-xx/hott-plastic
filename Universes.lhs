> module Universes (
>				Mk_U(..)
>				,UniverseLevel(..)
>				,has_solution
>				,has_name_in
>				,mk_u_NAME
>				,mk_u_elim
>				,mk_u_name
>				,mk_u_step
>				,mk_uid
>				,split_uid
>		) where

stuff relating to the above.

> import Data.Char(isDigit, isSpace)
> import Data.Maybe(fromMaybe)

> import Base(OkF(..), isOk, elimOk, fromOk)

> import Terms(Term(..), Type(..), FlatTerm(..), nameFromTerm, cAST)

<> import Context
<> import ContextOps

> import SharedSyntax(Name(..), BoundName(..), Extension(..), Ident(..))
> import SharedSyntax(Mk_U(..), UniverseLevel(..))
> import SharedSyntax(mk_uid, univ_separator, split_uid)

> import Reduction(dwhnf_to_ap)

> import Debugging (ftrace, show_term)


%-------------------------------------------------------------------------------

WARNING: chg-over from old Const mechanism, careful to distinguish with pure
	constr fns.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Building Universe-related Terms.

---

> mk_u_NAME :: Mk_U (OkF Term)					-- Type^i : Type
> mk_u_NAME p i
>  | i < 0 
>  = Fail $ "mk_u_NAME: negative universe index " ++ show i
>  | otherwise 
>  = Ok $ 
>    cAST (ContextVar $ mk_uid p i) 
>         Type

> mk_u_elim :: Ident -> Mk_U (OkF Term)			-- T^i : Type^i -> Type
> mk_u_elim univ_prefix p i
>  | i < 0 
>  = Fail $ "mk_u_elim: negative universe index " ++ show i
>  | otherwise 
>  = Ok $ mk_u_elim_ univ_prefix p i

> mk_u_elim_ univ_prefix p i		-- avoids check and ok-wrapping.
>  = cAST (Elim (mk_uid p i) 1) 
>         (DP Underscore (El $ fromOk $ mk_u_NAME univ_prefix i) Type)

> mk_u_name :: Ident -> Mk_U (OkF Term)			-- type^i : Type^i+1
> mk_u_name univ_prefix p i						-- T^i+1 (type^i) = Type^i 
>  | i < 0 
>  = Fail $ "mk_u_name: negative universe index " ++ show i
>  | otherwise 
>  = Ok $ 
>    cAST (Const (mk_uid p i) (tYPE i)) 
>         (El $ tYPE $ i + 1)
>    where
>		tYPE j = fromOk $ mk_u_NAME univ_prefix j

> mk_u_step :: Ident -> Ident -> Mk_U (OkF Term)-- t^i+1 : Type^i -> Type^i+1
> mk_u_step univ_prefix decode_prefix p i		-- T^i+1(t^i+1(a)) = T^i(a)
>  | i < 0 
>  = Fail $ "mk_u_step: negative universe index " ++ show i

>  | i == 0 	-- special case for 0.
>		-- t^0 : Prop -> Type^0
>		-- T^0(t^0(p)) = Prf p : Type
>  = Ok $ 
>    cAST (F_Const (mk_uid p i) t0_step_fn)
>         (DP Underscore (El $ ContextVar (Plain "Prop")) (El $ tYPE i)) 

>  | otherwise	-- standard case
>  = Ok $ 
>    cAST (F_Const (mk_uid p i) std_step_fn)
>         (DP Underscore (El $ tYPE $ i - 1) (El $ tYPE i)) 
>    
>    where
>	tYPE    = fromOk . mk_u_NAME univ_prefix
>	std_step_fn = E (mk_u_elim_ univ_prefix decode_prefix i)
>	    -- ie \t -> T^i(t), but written in simplest form (see t0 def)

>	t0_step_fn = {-\vs ->-} E (ContextVar $ Plain "Prf")  {-vs-}
>	    -- direct-encoded version of: [v:Prop]Prf v
>	    -- avoids extra beta redex, just in case.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Inference of Name existence in Certain Universes.

ie, checking restrictions and building insertions to Type^j

Term is a KIND - so will be El'd.
Same for univ? NB - good to keep repres. internal here (eg whether CVar or not)

---
Want to abstract out of Conv. any details of Universe repres. 
would like this to be another case called from conv. 
(NB this used at end? nothing before can hide it? 
  - maybe need analyse other uses of CASE.)

NB - can give arity of one, since we also give the Kind of the elim op.

> has_solution :: Term -> Term -> OkF Term
> has_solution (Ext 
>    (HardCast (Elim uT_i 1) 
>              (DP Underscore 
>                  (El (Ext (HardCast (ContextVar uType_i) Type))) 
>                  Type)))
>              ty
>  = ftrace ("GOT: " ++ show ty ++ "\n") ty `has_name_in` uType_i

> has_solution ut ty
>  = ftrace (s++"\n") $ Fail s
>    where s = unlines ["has_solution - no match", show ut, show ty]

---

> has_name_in :: Term -> Name -> OkF Term
> has_name_in ty n
>  = split_uid n >>= uncurry (hni ty)

> has_name_in _ _
>  = Fail "has_name_in - no match"

----

worry about prefix checking
 - can assume that TC means that we get univ_NAME here (ie, Type^i)

zero : Nat
zero : T^0(nat) 

> hni :: Type -> Ident -> UniverseLevel -> OkF Term
> hni ty p i
>  | null universe_levels         = tr $ Ok $ put_at_level i
>  | maximum universe_levels <= i = tr $ Ok $ put_at_level i 
>  | otherwise
>     = Fail $ "Type cannot be put at level (" ++ show i 
>				++ ") as it contains levels (" ++ show universe_levels ++ ")" 
>  where
>		tr = ftrace ("HNI = " ++ show (i, universe_levels) ++ "\n")
>		universe_levels = univs_in elim_prefix ty
>		mk_name = Plain $ "n" ++ show i ++ "-" ++ show_nicely ty
>		put_at_level j = cAST (Const mk_name ty) (El $ fromOk $ mk_u_NAME p j)

(El Nat) has_name_in Type^n...
acts like a type cast? result (El $ Type^0)
<> ([x:Type^0 ]x) zero;

---

Simple search through the ARGUMENTS 

> elim_prefix = "T" :: String			-- hard-coded for now

> univs_in :: String -> Term -> [UniverseLevel]
> univs_in e_name (El t) = univs_in e_name t

> univs_in e_name t
>  = case dwhnf_to_ap t of
>		Fail m     -> error $ "univs_in found abstr, "++ m ++"\n"++ show t
>		Ok (it,as) -> [ i | (Ap (Ext (HardCast (Elim n _) _)) _) <- as, 
>							let ok_pi = split_uid n, 
>							isOk ok_pi,
>							let (Ok (p,i)) = ok_pi,
>							p == e_name ]

<> univs_in e_name t
<>  = error $ "Got other term, " ++ show t

---

> show_nicely :: Term -> String
> show_nicely
>  = map rm_space . unwords . map show . just_it_and_Ts 
>    where
>		rm_space c = if isSpace c then '-' else c

> just_it_and_Ts t
>  = case dwhnf_to_ap t of
>		Fail m     -> error $ "just_it_and_Ts found abstr, "++ m ++"\n"++ show t
>		Ok (it,as) -> fromMaybe (Plain "bad_id") (nameFromTerm it) 
>		               : [ n | (Ap (Ext (HardCast (Elim n _) _)) _) <- as] 

> t0 = Ap (Var $ Plain "a") 
>         (Ap (cAST (Elim (Plain "T^3") 1) (Var $ Plain "3-type")) 
>             (Const (Plain "nat") (ContextVar $ Plain "Nat")))

> tx t = hni t "Type" 4
