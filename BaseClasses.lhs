>{-# LANGUAGE ScopedTypeVariables #-}
> module BaseClasses where

Collection of misc classes

with some hacks to avoid mutual rec in modules.

> import Prelude
> import Control.Exception

> infixl 9 `try_another`
> infixl 9 `try_another_`

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
`OkF' a type for returning errors. It is a monad & functor.

> type ErrorMessage = String
> data OkF a = Ok a | Fail ErrorMessage deriving Show


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
`Unpackable' represents reduction of a monad to an Ok or fail value. 
If some part of (m a) is to be shown in the Fail part, then user may need
to add type information to an expression to help with overloading.

> class Unpackable m where
>	unpack :: m a -> OkF a



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Class of monads who have some failure part.

NB this isn't necessarily a monad thing - though I wonder what possible uses
without could be.

1. they can return an error message
2. we can try another monad if the first fails.
2a. change the error message if an error has occurred.

3. pass error message on to other Fallible type

4. more generally, duplicate succeed/fail behaviour. - embed

5. change error to arbitrary fallible monad.

NOTE:
	split in 2 because ghc-2 complains. [it is a ghc-2.10 bug]

	division is maybe clearer, since Fallible is within-monad, whereas 
		Embeddable has strong notion of changing a monad.

NOTE:
	should there be Monad/Fallible superclasses? 

> class Fallible m where
>	fail_with_msg    :: String -> m a
>	try_another      :: m a -> (String -> m a) -> m a
>	try_another_     :: m a -> m a -> m a
>	change_msg       :: (String -> String) -> m a -> m a
>	prepend_msg      :: String -> m a -> m a

>	prepend_msg m = change_msg ((m ++ "\n") ++)
>	try_another_ a b = try_another a (const b)


> class Embeddable m where
>	rethrow       :: Fallible n => m a -> n b
>	embed         :: (Fallible n, Monad n) => m a -> n a
>	clarify_error :: (Fallible n) => (String -> String) -> m a -> n b 


> instance Fallible IO where
>--	let -- handler :: SomeException -> IO a'
>	try_another a b =  a `catch` \(e::SomeException) -> b (show e)

>	change_msg f a = error "Fallible.change_msg not defined for IO"
#ifndef __HASKELL98__
>	fail_with_msg = fail
#else 
>	fail_with_msg = ioError . userError
#endif

> instance Embeddable IO where
>	rethrow       = error "Embeddable.rethrow not defined for IO"
>	clarify_error = error "Embeddable.clarify_error  not defined for IO"
>	embed a       = error "Embeddable.embed UNDEFINABLE for IO"

Emble n, Monad n) => m a -> n abed for IO impossible because:
<> io_e :: (Fallible n, Monad n) => IO a -> n a
<> io_e a = (a >>= \x -> return x) `catch` \e -> fail_with_msg (show e)
To allow any monad as result, would need unsafePerformIO - which would be 
a bit UNSAFE. Hence, better to do explicit embedding, like liftIO_M_. 
Ie, only when we know the monad contains IO in some way.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Permutable:

allows monad b to move over another monad m.
good example is of shift over a Maybe value.
looks like, m has to have simple structure.

this is a kind of map operation BUT the order of monads in result is PERMUTED.

QQ: is there a possible default for really simple (linear/mono-constr?) monads?

> class Permutable m where
>   permute_map :: (Monad b, Functor b) => (a -> b c) -> m a -> b (m c)

more ops? eg plain_permute :: (Monad b?) -> m (b a) -> b (m a)
	- then could define permute_map in terms of this



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
`Forceable' 
  - is an extension of Eval and strict
  - but makes sure the argument is fully defined.
  - eg for strings, it can take the length of a string before applying the fn.
	 
#ifndef __HASKELL98__ 
> class Show a => Forceable a where
#else 
> class Forceable a where
#endif
>	force :: a -> a
>	fseq :: a -> b -> b
>	fseq x y = force x `seq` y

> instance Forceable a => Forceable [a] where
>	force [] = []
>	force (a:as) = force a `seq` force as `seq` a:as

> instance Forceable Char where
>	force a = a `seq` a
> instance Forceable Int where
>	force a = a `seq` a
