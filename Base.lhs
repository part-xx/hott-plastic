> module Base (
>			module Base, 
>			module BaseTypes, 
>			module BaseClasses,
>			module BaseFunctions,
>			module ReaderMonads,
>			module StateMonads
>		) where

just join the base stuff together

> import BaseTypes
> import BaseClasses
> import BaseFunctions
> import ReaderMonads
> import StateMonads

MAYBE: make this interface self-contained, eg export Maybe parts.

%-------------------------------------------------------------------------------
Misc Fns which involve several Base modules.

To DEBUGGING? NO! - it seems more than plain debugging? 

> assert_p :: (Fallible m, Monad m) => Bool -> String -> m ()
> assert_p p m 
>  = embed $ if p then Ok () else Fail m


%-------------------------------------------------------------------------------

#ifdef __HASKELL98__
> instance Show ((->) a b) where		-- grrrr
>	showsPrec _ f = showString "<<function>>"
#else
#  if __GLASGOW_HASKELL__ < 403 
#  else 
> instance Show ((->) a b) where		-- grrrr
>	showsPrec _ f = showString "<<function>>"
#  endif
#endif


#ifdef __HASKELL98__
#else 
> ($!) :: Eval a => (a -> b) -> a -> b
> ($!) = strict
#endif
