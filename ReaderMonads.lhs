> module ReaderMonads where

Plain and Error-handling Reader monad

> import BaseTypes
> import BaseClasses

#ifndef __HASKELL98__
#define fmap map
#endif

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
`RM' state reader monad for passing state around. 

Operations
 - read the fixed state
 - execute a monad in a changed state

State is never returned. Changes only propagate downwards.
The functionality is achieved by passing around a hidden argument. 
So, we make a monad instance for functions. 

> type RM s a = s -> a
> through_RM f = f
> extract_RM m = m

--> instance Functor ((->) s) where
-->	fmap f m = \s -> f (m s)

--> instance Monad ((->) s) where
-->	return a = \s -> a
-->	m >>= k  = \s -> k (m s) s

---
Now some general functions on this monad.

> runRM :: RM s a -> s -> a		-- elimination.
> runRM m s = extract_RM m s

> useStateRM :: (s -> a) -> RM s a
> useStateRM f = \s -> (f s)

> getStateRM :: RM s s 
> getStateRM = useStateRM id

> chgStateRM :: (s->s) -> RM s a -> RM s a
> chgStateRM sf m = \s -> m (sf s)



