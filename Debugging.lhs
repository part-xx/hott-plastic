#if 1 
> module Debugging (
>			trace,
>			pretty_crude_print,
>			module Debugging
#else
>			show_term,
>			tracef
#endif 
>			) where

Various routines, primarily for tracing, to help debugging.

#if __GLASGOW_HASKELL__ < 302
> import Trace
#else 
> import IOExts_(trace)
#endif

> import Pretty
> import PrettyAux

> import Base
> import Terms
> import TermOps(replaceTerminal)
> import Context 		-- (Ctxt, addCtxt_BV, findCtxt_DB, findCtxt)

> import Printing
> import PrintingAux


---
`show_term' - for debugging use.

> show_term c t = shortRender $ showTerm c t

---
`hack_show' 
  - show using ctxt mechanisms - only for use on fully closed terms.
  - include a test to divert to crude_print? 

> hack_show 
>  = longRender . showTerm (Ctxt fake_ctxt)
>    where
>		fake_ce = CE (Bind "Debug") (Var $ Plain "Debug")
#if 1 || PRODUCTION
>		fake_ctxt_length = 200
>		fake_ctxt = [ (Plain $ "x" ++ show n, fake_ce) 
>                    | n <- [1.. fake_ctxt_length ] ] -- NB finite ctxt.
#else
>		fake_ctxt = [ (Plain $ "x" ++ show n, fake_ce) 
>                    | n <- [1.. ] ]                  -- NB inf ctxt.
#endif

---

> show_mv_term :: Ctxt -> Term -> String
> show_mv_term c 
>  = show_term c . replaceTerminal rm_var_qm 
>    where
>		rm_var_qm (Var (Plain ('?':n)))
>		 = Just $ MetaVar $ Plain $ "new_mvar" ++ if null n then "" else n
>		rm_var_qm _ = Nothing


%-------------------------------------------------------------------------------

> ftrace :: String -> a -> a
> ftrace s a
>  = s `fseq` trace s a

> ftracef :: (a->String) -> a -> a
> ftracef f e = ftrace (f e) e

%-------------------------------------------------------------------------------
Common pattern, for inside some monadic comp.

> type TraceFn a = String -> a -> a
> type ShowFn    = Term -> String

> m_trace_tt 
>  :: Monad m => TraceFn (m ()) -> ShowFn -> String -> Maybe TypedTerm -> m ()
> m_trace_tt tf sf s Nothing 
>  = tf s $ 
>    return ()
> m_trace_tt tf sf s (Just (TT t ty))
>  = tf (unlines [s, indent ++ "== " ++ sf t, indent ++ ":: " ++ sf ty]) $
>    return ()
>    where
>		indent = replicate 4 ' '

