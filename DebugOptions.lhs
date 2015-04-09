>{-# LANGUAGE ScopedTypeVariables #-}
> module DebugOptions (
>			initialiseDebugOptions,
>			traceOnDebugFlag,
>			isDebugFlag_Set,
>			chgDebugOptions,
>			showDebugOptions,
>			DebugFlagName(..)
>		) where


Collection of Boolean flags, which turn various traces on or off.

This is a crude method of setting traces on the basis of a .rc file,
intended for debugging traces and the like.

For more principled options, use GlobalOptions.

RC FILE:
  - looks for ${PLASTIC_DEBUG_RC}, and tries that if set
  - otherwise tries ./plastic_debug.rc - for local settings
  - otherwise tries ${HOME}/.plastic_debug.rc

(NB - separate module, eg from Debugging.lhs, so it can be used ANYWHERE)


#if __GLASGOW_HASKELL__ < 302
> import Trace
#else
> import IOExts_(trace)
#endif

> import IOExts_ (unsafePerformIO)
> import IOExts_ (newIORef, readIORef, writeIORef, IORef)

> import Control.Exception

> import System.Environment
> import Data.Char(isAlpha, isSpace)
> import Data.List(sort)

> import BaseFunctions(sorted_elem)
> import Base(OkF(..))

#ifndef __HASKELL98__
#define fmap map
#endif

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Main functions.

---
`DebugFlagName'
  - being tidy here

> type DebugFlagName = String


---
`isDebugFlag_Set'
  - checks the flags from the rc file.
  - if compiling for PRODUCTION, then everything is False.

> isDebugFlag_Set :: DebugFlagName -> Bool
#ifdef PRODUCTION
> isDebugFlag_Set s = False
#else 
> isDebugFlag_Set "" = True		-- easy way to fix on.
> isDebugFlag_Set s  = is_in_flags s
#endif


---
`traceOnDebugFlag'
  - util function, gated by flag being set.

> traceOnDebugFlag :: DebugFlagName -> String -> a -> a
> traceOnDebugFlag s m x
#ifdef PRODUCTION
>  = x
#else 
>  = if isDebugFlag_Set s then trace m x else x
#endif


---

> chgDebugOptions :: ([String]->[String]) -> IO ()
> chgDebugOptions fn
>  = do
>		fs <- readIORef flags_ref
>		writeIORef flags_ref (sort $ fn fs)
>		return ()



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Where we store the flags.

> {-# NoInline flags_ref #-}
> flags_ref :: IORef [DebugFlagName]
> flags_ref = unsafePerformIO (newIORef [])

---
Use sorted lookup

#ifdef PRODUCTION
#else 
> {-# NoInline is_in_flags #-}
> is_in_flags :: DebugFlagName -> Bool
> is_in_flags s 
>  = s `sorted_elem` unsafePerformIO (readIORef flags_ref)
#endif

---

> read_rc_file :: IO (Maybe (String, String))
> read_rc_file
>  = do
>	home <- getEnv "HOME" `catch` \(e::SomeException) -> return "../../"
>	let rc_name = ".plastic_debug.rc"
>	let home_rc = home ++ "/" ++ rc_name
>	let use n = fmap (Just . ((,) n)) $ readFile n

>	ev <- try (getEnv "PLASTIC_DEBUG_RC") :: IO (Either SomeException String)
>	case ev of 
>		Right n -> use n
>		Left _  -> return Nothing

<>					ld <- Prelude.try (use rc_name)
<>					case ld of
<>						Right x -> return x
<>						Left _  -> use home_rc `catch` \_ -> return Nothing


> initialiseDebugOptions :: IO ()
> initialiseDebugOptions
>  = do
>	rc_file <- read_rc_file
>	case rc_file of
>		Just (name,opts) 
>		  -> do
>				putStr $ "Setting debug options from:" ++name++"\n"
>				writeIORef flags_ref (sort $ clean_up opts)
>				showDebugOptions
>		Nothing
>		  -> do
>				putStr $ "No debug options will be set.\n"
>				writeIORef flags_ref []
>				return ()


> showDebugOptions :: IO ()
> showDebugOptions
>  = do
>	os <- readIORef flags_ref
>	putStr $ unlines $ "Debug options are:" : map ("    "++) os 


---
`clean_up'
  - remove inactive lines and spurious spaces from rc file text

> clean_up 
>  = map (filter $ not . isSpace) . filter (any isAlpha . take 1) . lines 

