> {-# LANGUAGE ScopedTypeVariables #-}
> module Signals (
>		installSignalHandler,
>		preserveContext,
>		retrieveContext,
>		continueWithContext,
>		clearContexts

>		, sigINT				-- HACK for HUGS - and useful for ghc
>		, sigUSR1
>		, sigALRM

> 	) where

Contains the necessaries for resurrecting context on ^C etc.

Also used for implementing Undo.


> import IOExts_ (unsafePerformIO)
> import IOExts_ (newIORef, readIORef, writeIORef, IORef)


#undef __GLASGOW_HASKELL__ 
#if __GLASGOW_HASKELL__ < 302
> -- see patches below.
#else 
> import Posix
#endif


> import StateMonads
> import Context(Ctxt,emptyCtxt)
> import Base
> import Control.Exception


%-------------------------------------------------------------------------------
Some patches for hugs and old ghc.

#define PROGRAM "Hugs"

#if 1 || __GLASGOW_HASKELL__ == 210
	SOME HACKS TO MAKE OLD GHC LOOKS LIKE HUGS, ie SIGNALS-IGNORANT.
#undef __GLASGOW_HASKELL__
#undef PROGRAM
#define PROGRAM "ghc_2.10"
#endif

#ifndef __GLASGOW_HASKELL__ 
> type Signal = Int
> sigINT  =  2 :: Signal
> sigUSR1 = 16 :: Signal
> sigALRM = 14 :: Signal

> type SignalSet = [Int]
> fullSignalSet = [error "fullSignalSet not implemented in HUGS"]
> emptySignalSet = error "emptySignalSet not implemented in HUGS"

> installHandler _ _ _ 
>  = putStr ("** WARNING **\n    "++PROGRAM++" doesn't implement signals!\n\n")
> data Foo a = Catch a

> blockSignals   = return :: a -> IO a
> unBlockSignals = return:: a -> IO a
#endif

%-------------------------------------------------------------------------------

> installSignalHandler :: Signal -> IO () -> IO ()
> installSignalHandler s i
>  = do
>		-- putStrLn $ "Installing handler for " ++ show s
>		installHandler s (Catch i) (Just emptySignalSet) >> return ()


%-------------------------------------------------------------------------------
Place to store the Context(s), for use by the signal handler and Undo.

> contexts_to_keep = 100 :: Int

> add_to_contexts c cs = take contexts_to_keep $ c : cs

> get_nth_context :: Int -> [Ctxt] -> OkF (Ctxt, [Ctxt])
> get_nth_context n cs
>  | n < length cs  = Ok $ (cs !! n, drop n cs)
>  | otherwise      = Fail $ "Only have " ++ show (length cs) ++ " contexts."


---

> {-# NoInline context_ref #-}
> context_ref :: IORef [Ctxt]
> context_ref 
>  = unsafePerformIO $ newIORef [emptyCtxt]


%---------------------------------------
`retrieveContext'
  - retrieve the n'th context, and discard the more recent ones.

> retrieveContext :: Int -> M_ Ctxt ()
> retrieveContext n
>  = do
>		print_M_ $ "Undoing (" ++ show n ++ ") top level commands.\n"
>		ctxts <- liftIO_M_ $ readIORef context_ref
>		(ctxt,ctxts') <- embed $ get_nth_context n ctxts
>		liftIO_M_ $ writeIORef context_ref ctxts'
>		set_M_ ctxt

need to work back N copies - and reset these? or keep? 
Might want to store some info about it? eg undoing an undo of 20...

TREE? - and problems for multi context.

Undo 100 >> Undo 1 == id
ambiguty - about what kind of Undo. Hence UndoUndo? 

%---------------------------------------

NEED TO BLOCK SIGNALS? 

> preserveContext :: M_ Ctxt ()
> preserveContext
>  = do
>		c <- get_M_
>		cs <- liftIO_M_ $ readIORef context_ref
>		liftIO_M_ $ writeIORef context_ref (add_to_contexts c cs)
>		return ()


---
`continueWithContext'
  - explicitly block all signals until we're done.
  - could try to use SigSet in installHandler, but not sure if this is going
	to work with continuation-based handlers.
  - TODO - might want to specify new catch operation (maybe new composite run 
    op?) since the previous catch (probably) won't catch across a signal ctu?


> continueWithContext :: String -> (Ctxt -> IO a) -> IO a
> continueWithContext s ctu
>  = do
>		oss <- blockSignals fullSignalSet			-- block everything
>		putStrLn $ "Continuing with previous Ctxt at: " ++ s 
>		ctxts <- readIORef context_ref
>		let (ctxt, ctxts') = most_recent_context ctxts 
>		writeIORef context_ref ctxts'
>		_ <- unBlockSignals fullSignalSet -- WAS: oss						-- and return to prev state
>		ctu ctxt `catch` fail_cwc

> fail_cwc (e::SomeException) = error $ "FAILING in continueWithContext: " ++ show e ++ "\n\n"

---

> most_recent_context :: [Ctxt] -> (Ctxt, [Ctxt])
> most_recent_context cs
>  = elimOk (\m -> error $ "most_recent_context FAIL: " ++ m) id $
>    get_nth_context 0 cs


---
flush the stored contexts

> clearContexts :: M_ Ctxt ()
> clearContexts 
>  = liftIO_M_ $
>    do
>		putStrLn "Resetting Context to empty, and forgetting undo buffer."
>		writeIORef context_ref [emptyCtxt]

