> module StateMonads where

General State Transforming Monads.

> import BaseFunctions (warn)
> import BaseTypes
> import BaseClasses

#ifndef __HASKELL98__
#define fmap map
#endif

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Main Monad

A state-passing monad, with exceptions and restricted IO facility.

NOTES:
	The state is INSIDE the exception part, so failure means loss of state.
	This means that we don't have to worry about undoing the initial parts
	  of a failed operation.

	(NB I haven't algebraically proved that it is a monad...)

	EXCEPTIONS: not handled well. should probly have (Either IOError String)
	as the fail part. Lack of this causes a few ugly hacks, eg stopping when
	we have EOF. Theoretically, M_ should have exception behaviour of IO
	(use a class for this?)

> newtype M_ s a = M_ (s -> IO (OkF (s,a)))

> instance Functor (M_ s) where
>	fmap f (M_ sf) 
>	 = M_ $ \s -> sf s >>= \ok -> return $ fmap (\(s,a) -> (s, f a)) ok

> instance Monad (M_ s) where
>	return a = M_ $ \s -> return $ return (s,a)
>	(M_ sf) >>= k 
>	 = M_ $ \s -> sf s >>= 
>					elimOk (return.Fail) 
>						   (\(s,a) -> let (M_ kf) = k a in kf s)
>	fail m = M_ $ \s -> return (Fail m)

> instance Fallible (M_ s) where
>	fail_with_msg m     = M_ $ \s -> return (Fail m)
>	try_another         = try_M_
>	change_msg f (M_ m) = M_ $ \s -> fmap (change_msg f) (m s)


%-------------------------------------------------------------------------------
`print_M_' - show a string immediately.
`warn_M_' - show a string on stderr.

> print_M_ :: String -> M_ s ()
> print_M_ s = liftIO_M_ (putStr s)

> warn_M_ :: String -> M_ s ()
> warn_M_ s = liftIO_M_ (warn s)

---
`liftIO_M_'
  - only reformats - doesn't catch IO errors.

> liftIO_M_ :: IO a -> M_ s a
> liftIO_M_ i 
>  = M_ $ \s -> fmap (Ok . ((,) s)) i 

---
`liftOkF_IO_M_'
  - as above, but lifting the OkF return value in to (M_ s)

> liftOkF_IO_M_ :: IO (OkF a) -> M_ s a
> liftOkF_IO_M_ io = liftIO_M_ io >>= elimOk fail_with_msg return 


---
embed_M_
 - run one M_ from another, with different states.

> embed_M_ :: (from -> to) -> (to -> from) -> M_ to a -> M_ from a
> embed_M_ to from (M_ f)
>  = M_ $ \s -> f (to s) >>= \ok -> return (ok >>= \(s',a) -> Ok (from s', a))

---
`embed_other_M_'
  - like embed, but for running M_ whose state is UNRELATED to the outer
	state.

> embed_other_M_ :: s1 -> M_ s1 a -> M_ s0 a
> embed_other_M_ s0 (M_ mf)
>  = do
>		s <- get_M_
>		a <- liftOkF_IO_M_ (mf s0)
>		set_M_ s
>		return (snd a)

---
`raiseOkF_M_'
  - lift the implicit Ok/Fail to top level - making it visible.

> raiseOkF_M_ :: M_ s a -> M_ s (OkF a)
> raiseOkF_M_ (M_ m) 
>  = M_ $ \s -> m s >>= return . 
>						Ok . 
>						elimOk (\m -> (s, Fail m)) (\(s1,a) -> (s1, Ok a))

---
`get_M_' gets the state, 
`chg_M_' applies a change function to it (which may fail).
`set_M_' just changes it.

use_M_ does function which requires state
tfm_M_ does function which changes state as side effect

> get_M_ :: M_ s s
> get_M_ = M_ $ \s -> return (return (s,s))
> chg_M_ :: (s -> OkF s) -> M_ s ()
> chg_M_ f = M_ $ \s -> return (f s >>= \ok -> return (ok,()))
> set_M_ :: s -> M_ s ()
> set_M_ s = M_ $ \_ -> return $ return (s,())

> use_M_ :: (s -> OkF a) -> M_ s a
> use_M_ f = M_ $ \s -> return (f s >>= \a -> return (s, a))
> tfm_M_ :: (s -> OkF (s,a)) -> M_ s a
> tfm_M_ f = M_ $ \s -> return (f s >>= return)



---
`try_M_' attempts the first monad; if this fails, continue with the second,
	passing it the error message (so it can decide what to do with it).
	Thus, this implements exception handling at top level.
`catch_M_' is a convenience, of try_M_ with arg order reversed.

> try_M_ :: M_ s a -> (String -> M_ s a) -> M_ s a
> try_M_ (M_ f1) m2
>  = M_ $ \s -> f1 s >>= elimOk (fail_ s) (return.return)
>    where
>		fail_ s m = let (M_ mf) = m2 m in mf s

> catch_M_ = flip try_M_ 

---
`run_M_' runs a M_, returning an IO value.

> run_M_ :: M_ s a -> s -> IO (OkF(s,a))
> run_M_ (M_ f) s
>  = f s

---
`restart_M_' is run_M_, just name change to underline that it isn't
conceptually a clean run (eg, might later add other state to the monad.)

> restart_M_ = run_M_ 

---

> lift_ST_ :: ST_ s a -> M_ s a
> lift_ST_ sf
>  = M_ $ \s -> let (s2,a) = sf s in return (return (s2,a)) 


---

> switch_M_ :: Show a => M_ s a -> M_ s String
> switch_M_ (M_ f)
>  = M_ $ \s -> f s >>= \ok -> return (switch_ s ok)

> switch_ s (Ok (s',v)) = Fail $ show v
> switch_ s (Fail m)    = Ok (s,m) 


---
`atomicM_'
  - only accept state change if the monad doesn't fail. 
  - otherwise return with the initial state.

  - NB is this really required, since State is INSIDE the OkF? any failure
	without a catch will mean failure back to the previous catch. 
	See while_M_

> atomicM_ :: M_ s a -> M_ s a
> atomicM_ m
>  = do
>		s <- get_M_ 
>		m `try_M_` \e -> set_M_ s >> fail_with_msg (e ++ "\n" ++ msg)
>    where
>		msg = "Returning to original state.\n"


---
`while_M_'
  - iterate the function until it returns a failure.
  - collect the results AND STATE up to that point.
  - NB this has effect of bringing state outside the Okf. 

> while_M_ :: (a -> M_ s b) -> [a] -> M_ s [b]
> while_M_ mf [] = return []
> while_M_ mf (x:xs)
>  = let ~(M_ mx) = mf x 
>        ~(M_ mxs) = while_M_ mf xs 
>    in
>    M_ $ \s -> 
>         mx s >>= \ok -> 
>         case ok of
>            Fail _    -> return $ Ok (s, [])
>            Ok (s2,b) -> mxs s2 >>= \ok2 -> 
>                         return $
>                         elimOk Fail (\(s,bs) -> Ok (s,b : bs)) ok2


DISCUSSION:  merits of having state inside or outside the OkF monad
----------

INS: we don't have to worry about discarding state chgs from failed operations
OUT: while_M_ is horrible!



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Testing

#if 0
> r :: IO ()
> r = run_M_ tm 0 >>= print

> tm :: M_ Int ()
> tm 
>  = do
>		print_M_ "hi"
>		chg_M_ (Ok . (+3))
>		try_M_ tm1 (const tm2)

> foo2 :: (s->s) -> M_ s ()
> foo2 f = sequence $ copy 1000 $ chg_M_ (Ok . f)

> tm1 
>  = sequence (copy 10 $ get_M_ >>= \s -> print_M_ ("one-" ++ show s) >> foo2 (+1)) >>
>	 fail_with_msg "end tm1"

> tm2
>  = sequence (copy 10 $ get_M_ >>= \s -> print_M_ ("two-" ++ show s) >> foo2 (+1)) >>
>    fail_with_msg "end tm2"

> copy n = take n . repeat

#endif
