> module BaseTypes (module BaseTypes, OkF(..)) where

Collection of misc definitions and types.

> import Data.Maybe
> import BaseClasses

#ifndef __HASKELL98__ 
#define fmap map
#endif

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
`STM' - type of state transformations returning through monad.
This is an informal synonym, to make type sig's more informative.

> type STM s m r = s -> m (s,r)

---
`ST_' is a basic state transformation, ie no monad.

> type ST_ s r = s -> (s,r)




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
`OkF' a type for returning errors. It is a monad & functor.

<defined in BaseClasses to avoid mutual rec in Unpackable>

> instance Functor OkF where
>	fmap f (Ok a)   = Ok $ f a
>	fmap f (Fail m) = Fail m    -- need rebuild, for type 

> instance Monad OkF where
> 	Fail m >>= f = Fail m
>	Ok x   >>= f = f x
>	return           = Ok

#ifdef __HASKELL98__
>	fail = Fail 
#else 
> instance MonadZero OkF where
> 	zero = Fail "monad zero"
#endif

> instance Unpackable OkF where
>	unpack = id

> instance Fallible OkF where
>	fail_with_msg = Fail
>	try_another (Fail s)  m = m s
>	try_another ok@(Ok _) m = ok 
>	change_msg f (Fail m) = Fail $ f m
>	change_msg f ok       = ok

> instance Embeddable OkF where
>	rethrow (Fail m) = fail_with_msg m
>	rethrow _        = error "rethrow got Ok in OkF"
>	embed (Ok a)     = return a 
>	embed (Fail m)   = fail_with_msg m
>	clarify_error f (Fail m) = fail_with_msg $ f m
>	clarify_error f ok       = error "clarify_error got Ok in OkF"

---
and OkF is Permutable

> instance Permutable OkF where
>	permute_map f (Ok x)   = fmap Ok (f x)		-- use fmap now.
>	permute_map f (Fail m) = return (Fail m)

---
`isOk', `isFail' - obvious predicates.

> isOk (Ok _) = True
> isOk _      = False
> isFail (Fail _) = True
> isFail _        = False

---
fromOk, fromFail

> fromOk   (Ok x)   = x
> fromOk   (Fail m) = error $ "fromOk on Fail: " ++ m
> fromFail (Fail m) = m
> fromFail (Ok x)   = error "fromFail on Ok."

---
`failAgain' encapsulates process of propagating a fail message through
different OkF types. 

MAYBE: have this as part of Fallible class? 

> failAgain :: OkF a -> OkF b
> failAgain (Fail m) = Fail m
> failAgain ok       = error "failAgain ok"

---
`elimOk' reduces OkF a to a single type b. 
Eg elimOk id show :: Show a => OkF a -> String

> elimOk :: (String -> b) -> (a -> b) -> OkF a -> b
> elimOk fail_f ok_f ok
>  = case ok of
>		Ok a   -> ok_f a
>		Fail m -> fail_f m

---
`okfToBool' - useful combination.

> okfToBool :: OkF a -> Bool
> okfToBool = elimOk (const False) (const True)



%-------------------------------------------------------------------------------
`maybeToOkF' changes a Maybe to an Okf, incorporating the error message if
it is a Nothing.

> maybeToOkF :: String -> Maybe a -> OkF a
> maybeToOkF e = fromMaybe (Fail e) . fmap Ok		-- use fmap now

---
`testToMaybe' converts a bool test to a Maybe.

> testToMaybe :: (a -> Bool) -> a -> Maybe a
> testToMaybe p x
>  | p x       = Just x
>  | otherwise = Nothing

---
`showMaybeString' shows a showable thing as plain string rather than quoted
string. 

Surely there is a better/general solution to this? 

> showMaybeString :: Show a => Maybe a -> String
> showMaybeString Nothing  = "Nothing"
> showMaybeString (Just x) = "(Just " ++ showsPrec 1 x ")"


---
Maybe is Permutable

> instance Permutable Maybe where
>	permute_map f (Just x) = fmap Just (f x)
>	permute_map f Nothing  = return Nothing


---
Maybe is Fallible

> instance Fallible Maybe where
>	fail_with_msg _ = Nothing
>	try_another Nothing    m = m "Nothing from Maybe"
>	try_another j@(Just _) m = j
>	change_msg f Nothing    = Nothing
>	change_msg f j@(Just _) = j

> instance Embeddable Maybe where
>	rethrow Nothing  = fail_with_msg "from Nothing"
>	rethrow (Just _) = error "rethrow got Just in Maybe"
>	embed Nothing  = fail_with_msg "from Nothing"
>	embed (Just x) = return x
>	clarify_error f Nothing  = fail_with_msg $ f "<Nothing>"
>	clarify_error f (Just _) = error "clarify_error got Just in Maybe"


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Either is sometimes useful as a monad.

--> instance Monad (Either a) where
-->	Left m  >>= f = Left m
-->	Right v >>= f = f v
-->	return        = Right

> instance Show a => Unpackable (Either a) where
>	unpack (Right x) = Ok x
>	unpack (Left y)  = Fail $ show y		-- relies on overloading.

<> isRight (Right _) = True
<> isRight _         = False


