>{-# LANGUAGE ScopedTypeVariables #-}
> module Plastic (
>            top_loop,            -- nb could lift all of this? the IO stuff?

>            parseThenCall,
>            IM(..)

>        ) where

Testing the parser TOGETHER with context commands and term translation.

--> import qualified Prelude (catch)

> import Prelude
> import Data.List hiding (find)
> import Data.Maybe
> import Data.Char (isDigit,isSpace,ord,isPrint)
> import System.Environment(getEnv)
> import System.Process(system)
> import System.Exit(exitFailure, exitWith, ExitCode(..))

--> import System(exitFailure, exitWith, ExitCode(..))

> import Control.Monad (foldM)
> import System.IO(stdin)
> import System.IO.Error(ioeGetErrorString, ioeGetHandle, isEOFError)
> import IOExts_ (newIORef, readIORef, writeIORef, IORef, unsafePerformIO)

> import Control.Exception

> import Base

> import SimpleTerm
> import CommandTypes
> import LF_Parser(parseThenCall, parseTermThenCall, SyntaxInfo(..))

> import Terms
> import TermReduction(reduceAppliedAbstrs)
> import SimpleToTerm
> import Context
> import ContextCmds
> import ContextOps (contextChanges, showCtxtDiffs, update_def_in_Ctxt)
> import ContextOps (restrictCoercions)


> import qualified UniverseCmds(process)

> import Reduction
> import TypeInference
> import Convertibility (test_conv_or_unify)

> import ComputationRules (simple_compileElimRules)
> import Inductive

> import Refine
> import IntrosReturn (intros, returnAllIntros, returnIntros)
> import Tactics        -- SHOULD RE-EXPORT Refine? 

> import Printing

> import Case

> import CoercionCmds(addCoercion, showCoercions)

> import MetaVars(test_CTC, definition_MetaVars, simple_MetaVars)

> import UserSyntax


---

> import Input
> import GlobalOptions (showOptionSettings, showOptionInfo)
> import GlobalOptions (setInitialOptions, setOneOption)
> import GlobalOptions (proofGeneralMode, p_Normal)
> import Signals
> import Debugging(trace)
> import DebugOptions(initialiseDebugOptions, showDebugOptions)
> import DebugOptions(isDebugFlag_Set, chgDebugOptions)


#undef __GLASGOW_HASKELL__
#ifdef __GLASGOW_HASKELL__
> import Posix(getProcessTimes, userTime, elapsedTime, ProcessTimes, ClockTick)
> import Posix(getSysVar, SysVar(ClockTick))

> import Concurrent(myThreadId, ThreadId)

# if __GLASGOW_HASKELL__ < 500 
> import Concurrent(raiseInThread)
> import Exception ( Exception(IOException)
>                  , blockAsyncExceptions
>                  , justIoErrors
>                  , justErrors
>                  , catchIO
>                  , tryIO)
# else 
> import Concurrent(throwTo)
> import Exception ( Exception(IOException)
>                  , ioErrors
>                  , errorCalls
>                  , catchJust
>                  , tryJust)
#  if __GLASGOW_HASKELL__ < 504 
> import Exception ( blockAsyncExceptions )
#  else 
> import Exception ( block )
#   define blockAsyncExceptions block
#  endif 
> catchIO = catchJust
> tryIO = tryJust
> justIoErrors = ioErrors
> justErrors = errorCalls
> raiseInThread = throwTo
# endif 

#else 

> type ThreadId = Int
> data Exception = IOException IOError
> raiseInThread = error "unimplemented"
> blockAsyncExceptions = error "unimplemented"
> justIoErrors = error "unimplemented"
> catchAllIO :: Control.Exception.Exception e => IO a -> (e -> IO a) -> IO a
> catchAllIO = catch
> catchIO x = catch
> myThreadId :: IO ThreadId
> myThreadId = return (error "unimplemented")

#endif

---
TMP

> line_width = 80 :: Int        -- CHANGE? or FLAGS? 




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
MAIN LOOP

---
`ms', `msi'
  - takes string and runs it as if from stdin
  - msi reads from stdin afterwards, thus string can set context

> ms s 
>  = do c <- start_and_finish (error "no thread") (string_loop s) emptyCtxt 
>       show_final_ctxt c

> msi s
>  = do c <- start_and_finish (error "no thread") (string_loop s >> r_loop) emptyCtxt 
>       show_final_ctxt c

---
`string_loop'
  - run input as if it was typed in
  - need to append a semicolon, so it looks like keyboard input.

> string_loop :: String -> IM
> string_loop s
>  = do is <- preprocessString [] (s ++ ";")
>       i_loop is

---

> show_final_ctxt :: OkF Ctxt -> IO ()
> show_final_ctxt (Fail m) 
>  = putStr m
> show_final_ctxt (Ok c)
>  = putStr $ "Final Ctxt:\n" ++ showCtxt Show_Everything c ++ "\n\n\n"

---
TMP TESTS

> t0 = let s = "[A:Type][a:A][g:(T:Type)T->T][y=g ?t a]g ?t y" in 
>        putStr (s ++ "\n") >> ms s >> putStr (s ++ "\n")

> t1 = let s = format [ "[A:Type][a:A][g:(T:Type)T->T]",
>                        "Claim x:A",
>                        "[y=g ?t a]",
>                        "Refine g ?t y"
>                        ] in 
>        putStr (s ++ "\n") >> msi s 

> t2 = let s = format [ "[A:Type][a:A][g:(T:Type)T->T]",
>                        "Claim v: A-> A",
>                        "Intros u",
>                        "[x=g ? u]",
>                        "[y=g ? x]",
>                        "[z=g ? y]",
>                        "Refine g ? z",
>                        "Return"
>                        ] in 
>        putStr (s ++ "\n") >> msi s 

> t4 = let s = "[A,B:Type][a:A][f:A->B][g:B->Type]\nCoercion f\ng a" in
>        putStr (s ++ "\n") >> msi s

> t5 = let s = format [  "[A,B,C,D:Type][a:A][e:D->Type]"
>                        ,"[f:A->B][g:B->C][h:C->D]"
>                        -- ,"DEBUG CO_DECL_TRACING"
>                        ,"SET verbosity Everything"
>                        ,"Coercion = f"
>                        ,"Coercion = g"
>                        ,"Coercion = h"
>                        ,"Ctxt 10"
>                        ,"e a"
>                        ,""
>                        ] in 
>        putStr (s ++ "\n") >> msi s 

> t6 = let s = format [  "[A,B,C:Type][a:A][g:(T,U:Type)T->U]"
>                        ,"DEBUG CO_DECL_TRACING"
>                        ,"SET verbosity Everything"
>                        ,"Coercion Parameters [T:Type] = g T A"
>                        ,"Ctxt 10"
>                        ,"Coercion Parameters [T:Type] = g T B"
>                        ,"Coercion Parameters          = g B C"
>                        ,"g C A a"
>                        ,""
>                        ] in 
>        putStr (s ++ "\n") >> msi s 

> t7 = let s = format [ "import ../lib/Sol/Sol.lf"
>                        -- , "DEBUG SYNTHESIS_TRACING SYNTHESIS_AP_TRACING CONV_TRACING"
>                        , "Inductive [A:Type] [eq : (x:El A)(y:El A)El Prop] Relation Constructors [eqr : (a:El A)Prf(eq a a)]"
>                        ] in 
>        putStr (s ++ "\n") >> msi s 

> format = unlines . map (++";")


---

> m = top_loop 

> top_loop :: IO ()
> top_loop 
>  = do
>        my_thread <- myThreadId
>        setInitialOptions
>        installSignalHandler sigALRM exitFailure 
>        start_and_finish my_thread r_loop emptyCtxt
>        return ()

> start_and_finish :: ThreadId -> IM -> Ctxt -> IO (OkF Ctxt)
> start_and_finish my_thread loop ctxt 
>  = do
>        initialiseDebugOptions
>        installSignalHandler sigUSR1 (interrupt my_thread)
>        installSignalHandler sigINT  (interrupt my_thread)
>        let run c = catchIO justIoErrors
>                    (installSignalHandler sigINT  (interrupt my_thread) >>
>                     run_M_ (loop `try_M_` say_fail "top loop") c)
>                    (\e -> do
>                            putStrLn ("Caught Exception: " ++ show e)
>                            if (stop_at e) 
>                                then return (Fail $ "Because: " ++ show e)
>                                else continueWithContext "MAIN LOOP\n" run
>                    )
>        ok <- run ctxt
>        case ok of 
>            Fail m   -> return (Fail $ "Global Fail: " ++ m)
>            Ok (c,_) -> return (Ok c)
>    where
>        interrupt t = blockAsyncExceptions $
>                      raiseInThread t (IOException thing)
>        thing = userError "caught interrupt"

>        stop_at e 
>         = isEOFError e && isJust handle && fromJust handle == stdin
>           where handle = ioeGetHandle e
>            -- stop if an EOF error.

---
`say_fail'
  - use this to notify failure and subsequent continuation
  - NB testing DEPENDS on this standardised form of report.

> say_fail :: String -> (String -> IM)
> say_fail s
>  = \e -> print_M_ ("FAIL: (" ++ s ++ ")\n" ++ e ++ "\n") 


---
top level loops.

read one ;-string from stdin, with prompt.
stop if there's a problem with reading.
stop if first part of cmd says "exit".

> r_loop :: IM
> r_loop 
>  = do
>        fs <- use_M_ (Ok . filesInCtxt)
>        m_is <- processSemicolonLine fs
>        case m_is of
>            Nothing                          -> return ()
>            Just (I_Cmd c _:_) | want_exit c -> return ()
>            Just is                          -> do { i_loop is ; r_loop }
>    where
>        want_exit i = unwords (take 1 $ words i) `elem` ["xx"]



---
`i_loop'
  - for sequence of cmds from stdin
  - if any fails, then just report it and continue with next.
  - NB can handle several cmds, even though `loop' above supplies just one
  - it could be useful for other kind of interface? (eg dV).

> i_loop :: [Input] -> IM

> i_loop [] 
>  = return ()

> i_loop is@(I_File n I_File_Start : _)
>  = do
>       liftOkF_IO_M_ $ setOneOption "verbosity" (Just "Quiet")
>       f_loop [] is `try_M_` say_fail "file reading"
>       liftOkF_IO_M_ $ setOneOption "verbosity" (Just "Normal")
>       preserveContext -- store Ctxt after an import.

> i_loop (I_File n I_File_Present : is)
>  = do
>       preserveContext 
>         -- store Ctxt, just for synchronisation. - ie, import is no-op
>	i_loop is

> i_loop (i:is)
>  = do
>       one_line preserveContext i 
>             `try_M_` say_fail "one line"        -- catch any uncaught error
>       i_loop is

---
`f_loop' 
  - special handling for file input.
  - maintains stack of files being read.
  - main difference to i_loop is that failure in one file means failure for
    the remaining input stream.
  - (in principle could implement, fail in file means try loading siblings).

> f_loop :: [FilePath] -> [Input] -> IM

<> f_loop [] _                         -- not really needed.
<>  = fail_with_msg $ "ERROR - no files in f_loop!"

> f_loop ns [] 
>  = fail_with_msg $ "file end problem in f_loop, files " ++ show ns ++ ""

> f_loop ns (I_File n I_File_Start : is)
>  = do
>        pg <- liftIO_M_ $ proofGeneralMode
>        p_Normal $ if pg then eager_start else ""
>        p_Normal $ "(Processing file " ++ n ++ ")\n"
>        chg_M_ (Ok . openFileInCtxt n)
>        f_loop (n:ns) is
>    where
>        eager_start = "\254"

> f_loop ns (I_File n I_File_Present : is)
>  = do
>        p_Normal $ "(File " ++ n ++ " already processed)"
>        chg_M_ (Ok . fileDepInCtxt n)
>        f_loop ns is

> f_loop ns (I_File n I_File_End : is)
>  = do
>        p_Normal $ "(Finished file " ++ n ++ ")\n"
>        pg <- liftIO_M_ $ proofGeneralMode
>        p_Normal $ if pg then eager_end   else ""
>        chg_M_ (Ok . closeFileInCtxt n)
>        rebuildLookupTable
>        if (length ns == 1)
>            then i_loop is 
>            else f_loop (ns \\ [n]) is
>    where
>        eager_end   = "\255"

> f_loop ns (i:is) 
>  = do
>        optional_echo_cmds i
>        one_line don't_preserve_ctxt i `try_M_` file_loop_fail ns i
>        f_loop ns is

---
`file_loop_fail'
  - show informative error mesg.
  - and quit loop if flag is on.
  - NB maybe this option should be in GlobalOptions? 


> file_loop_fail ns i m
>  = do
>        say_fail "file loop" (m ++ "\n" ++ file_loc ++ which_line i)
>        if ctu_on_file_error
>            then do
>                    print_M_ "Continuting with file\n"
>                    return ()
>            else do
>                    print_M_ "*** Jumping to top-level loop ***\n"
>                    r_loop
>                    -- elimOk (error "file_loop_fail") id (tmp_cmd "toploop" [])
>                    fail_with_msg "file loop, with stop on fail"
>    where
>        ctu_on_file_error = isDebugFlag_Set "CTU_ON_FILE_ERROR"
>        file_loc = "Location:\n" ++ unlines [ "\t" ++ n | n <- ns ]
>        which_line (I_Cmd s ss) = "While processing:\n" ++ unlines ss
>        which_line i            = "While processing: " ++ show i

---
Show the cmds on screen
 - might be needed for hugs.

> optional_echo_cmds (I_Cmd c ss) = return ()
> -- optional_echo_cmds (I_Cmd c ss) = print_M_ (unlines ss)    -- echo cmd here
> optional_echo_cmds i            = return ()





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
`one_line' 
  - handles one input clause.
  - this level just does decoding of &-abbreviations and handles comments.
  - eventually calls one_line_ to do the real processing.

  - IMPORTANT: all abbreviations must be translated to an explicit full
    command for the context preservation and Undo to work properly.

  - IMPORTANT: must_fail, if it works, produces an undoable command. hence
    we have to preserve the context if the must_fail fails as expected.

  - IMPORTANT: lines starting with "&S" do not have the context preserved
    after they complete. This is important for ProofGeneral synchonisation.


> one_line :: CtxtPreserver -> Input -> CM ()
> one_line _ (I_Cmt "") = p_Normal "\n"
> one_line _ (I_Cmt s)  
>  | length s < line_width - 10 = p_Normal $ "\n===> " ++ s ++ " <===\n\n"
>  | otherwise                  = p_Normal $ "\n" ++ s ++ "\n\n"

> one_line _ (I_Cmd c ss)
>  | all isSpace c    = return ()

> one_line preserve_or_not (I_Cmd ('&':c) ss)
>     -- SPECIAL CMDS
>  | null c                                         -- "&" = abbrev for Ctxt
>     = one_line preserve_or_not (I_Cmd "Ctxt" [])

>  | take 1 c == "p"                                -- parsing test
>     = one_line preserve_or_not 
>                (I_Cmd ("JustParse " ++ drop 1 c) ("JustParse":ss))

>  | take 1 c == "P"                                -- parse MUST fail
>     = must_fail preserve_or_not
>     $ one_line  preserve_or_not $ I_Cmd ("&p " ++ drop 1 c) ss

>  | take 1 c == "F"                                -- cmd MUST fail
>     = must_fail preserve_or_not
>     $ one_line  preserve_or_not $ I_Cmd (drop 1 c) ss

>  | take 1 c == "E"                                -- echo the cmd, for debug.
>     = do
>            print_M_ (unlines ss)
>            one_line preserve_or_not $ I_Cmd (drop 1 c) ss

>  | take 1 c == "S" -- silent.
>     = one_line_ don't_preserve_ctxt $ I_Cmd (drop 1 c) ss
>       -- don't keep ctxt for this

>  | take 1 c == "t" -- timing
>     = do
>            p1 <- liftIO_M_ getProcessTimes
>            one_line_ preserve_or_not $ I_Cmd (drop 1 c) ss
>            liftIO_M_ $ reportTimes p1

>  | otherwise 
>     = one_line preserve_or_not (I_Cmd ("Ctxt " ++ c) ss)   
>       -- interp as ctxt call

> -- this code is duplicated below
> -- this instance allows unparseable args (since it is pre-parsing)
> -- keeping this one, because settings are a Meta notion
> one_line _ (I_Cmd c ss)
>  | length ws == 2 && head ws == "SET" 
>     = liftOkF_IO_M_ $ setOneOption (ws !! 1) Nothing
>  | length ws == 3 && head ws == "SET"
>     = liftOkF_IO_M_ $ setOneOption (ws !! 1) (Just $ ws !! 2)
>  where
>    ws = words c

> one_line preserve_or_not i@(I_Cmd c ss)             -- call aux.
>  = one_line_ preserve_or_not i

> one_line _ i
>  = fail_with_msg $ "Unknown input form: " ++ show i


---
`CtxtPreserver'
  - action for saving context (or not) 
  - file loop doesn't preserve
  - normal input does.
  - Note: the &F etc requires chg from default policy

> type CtxtPreserver = CM ()
> don't_preserve_ctxt :: CtxtPreserver
> don't_preserve_ctxt = return ()
> -- preserveContext - imported from Signals.lhs



---
`one_line_'
  - parse a line, execute the command therein, and preserve ctxt if reqd.
  - catch any calls to error whilst executing the cmd

> one_line_ preserve_or_not (I_Cmd c ss)
>  = catch_error_calls $
>    do
>       parseThenCall do_cmd (do_cmd . Term) parseTables c
>	preserve_or_not


---
`must_fail'
  - if a command fails, then must_fail returns success. And vice versa.
  - the context must be preserved if the command fails as expected, since
    the command is regarded in ProofGeneral as having succeeded.

> must_fail :: CtxtPreserver -> IM -> IM
> must_fail preserve_or_not m 
>  = catch_M_ (\m -> fail_with_msg ("must_fail didn't, " ++ m ++ "\n")) $
>    do
>        e <- switch_M_ m
>        p_Normal $ "Failed as expected:"
>        p_Normal e
>        preserve_or_not



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Dispatch for Commands

Each of these returns a string which contains relevant info to user, and it
can also control Ctxt return.

NO - they each print what they feel like. 

---

Could replace this despatch mechanism with direct calls in the parser - there
is no deep reason for returning an algebraic type. 

> do_cmd :: TopCommand -> IM

> do_cmd (Term x)   
>  = do
>        case is_tmp_cmd x of
>            Ok m   -> m
>            Fail s -> do_term Nothing x

> do_cmd (Normal x) = do_term (Just $ \t -> use_M_ $ \c -> Ok $ hnf c t) x

> do_cmd (Conv o cs l r) = do_conv o cs l r

> do_cmd (Ctxt_op ref)
>  = do
>        let to_show = case ref of
>                        Nothing -> Show_Everything 
>                        Just (Left id) -> Show_Entry id
>                        Just (Right n) -> Show_Some n
>        c <- get_M_ 
>        p_Normal $ showCtxt to_show c

> do_cmd (Tactic (Basic (Intros r r_term vs))) 
>  = do
>        let act = case r_term of
>                    Nothing -> return ()
>                    Just t  -> refine r t -- refine by additional term first
>        intros r act vs
>        show_current_ctxt

> do_cmd (Tactic (Basic (Refine r t)))    = refine r t >> show_current_ctxt
> do_cmd (Tactic (Basic (Equiv r t)))     = equiv_tactic r t >> show_current_ctxt

> do_cmd (Tactic (Basic (Induction r v))) = induction r v
> do_cmd (Tactic (Basic (Immed r)))       = immed_tactic r
> do_cmd (Tactic (Basic (Assumption r)))  = print_M_ $ "Tactic Assumption NYI"

> do_cmd (Tactic (Then_T t1 t2))
>  = do
>    do_cmd (Tactic t1) 
>    do_cmd (Tactic t2) 

> do_cmd (Tactic t)
>  = fail_with_msg $ "general tactics not yet implemented:\n" ++ show t ++ "\n"

> do_cmd (Defs_Decls ds)  = add_defs_decls ds
> do_cmd (Local_Defs ds)  = error "NYI properly" -- add_local_defs ds

> do_cmd (Claim PlainClaim opt_id t) = claim opt_id t
> do_cmd (Claim GoalClaim opt_id t) = fake_goal opt_id t
>    -- corresponding killref is elsewhere



---

> do_cmd (Cut cs)                = mapM_ cut cs 

> do_cmd i@(Inductive _ _ _ _)   = processInductive i

> do_cmd (SimpleElimRule it eo ea rs) 
>  = simple_compileElimRules it eo ea rs    -- TEMP WORKAROUND!

> do_cmd i@(FunctionDef f)       = processFunctionDef f

> do_cmd (ModuleHeader i)        = return ()  -- ignore - see preprocessing...

> do_cmd c@(CoercionDecl{})      = addCoercion c

<> do_cmd (ElimRule n rs)        = compileElimRules n rs        - POSTPONED!


> do_cmd (JustParse (Term t)) 
>  = print_M_ $ "Term Parse ok, " ++ showSimpleTerm t ++ "\n"
> do_cmd (JustParse c@(JustParse _))
>  = print_M_ "Recursive " >> do_cmd c
> do_cmd (JustParse cmd)
>  = print_M_ $ "Command Parse ok, " ++ show cmd ++ "\n"
>        -- Just by getting here, the parse is valid. So just show results.
>        -- Failure in parsing is caught by the general mechanism.


> do_cmd u@(InfiniteUniv _ _ _ _) = UniverseCmds.process u
> do_cmd u@(FiniteUniv _)         = UniverseCmds.process u
> do_cmd u@(AddToUniverse _ _)    = UniverseCmds.process u

> do_cmd cmd = error $ "Unknown command " ++ show cmd

--- 
aux fn.
    - use Show_Metavars? 

> show_current_ctxt
>  = do c <- get_M_
>       p_Normal $ showCtxt Show_MetaVars c


%---------------------------------------
`Temporary Commands'

This mechanism allows quick impl of commands without having to change the
parser (which means a lot of compilation...)

Relies on fact that most commands look like an id applied to a list of terms.
Hence decompose the term to this format, and see what matches.

The interface distinguishes between some valid IM, and a failure to recognise
a tmp command.

> type Is_Tmp_Command = OkF IM

> is_tmp_cmd :: SimpleTerm -> Is_Tmp_Command
> is_tmp_cmd t
>  = do
>        (n,ts) <- decomposeTmpCmd t 
>        tmp_cmd (show n) ts

---

> tmp_cmd :: String -> [SimpleTerm] -> Is_Tmp_Command

> tmp_cmd "exit" _
>  = Ok $
>    do
>        print_M_ "*** Exiting... ***\n"
>        liftIO_M_ $ exitWith ExitSuccess

> tmp_cmd maybe_die _
>  | "die" `isPrefixOf` dropWhile (not.isPrint) maybe_die
>     = Ok $
>        do
>            print_M_ "*** Abandoning everything... ***\n"
>            error "Crashed at user's behest!"
>        -- the extended test catches cases when bad chars get in to input.

> tmp_cmd "toploop" _        -- quick escape to top level, and interactive
>  = Ok $
>    do
>        print_M_ "*** Escaping to top loop, ignoring rest of input. ***\n"
>        r_loop 
>        fromOk $ tmp_cmd "exit" []    -- make sure we exit

TOPLOOP - this works - for local loop call. SO, won't exit do same? ie, 
can't be sure that we do jump to end. 
BUT - since all r_loop calls will get back to exit, then maybe it is ok? 

CHECK - still works since now using  "exitWith ExitSuccess" ? 

> tmp_cmd "Symbol" ns
>  = Ok $
>    print_M_ $ "Symbol not implemented - ignoring for " ++ show ns ++ "\n\n"
>    -- this establishes 'sugaring' transformations.

QQ: anything else to be done here? 

> tmp_cmd "Forget" ts
>  = Ok $
>    forget [ n | (Name_S n) <- ts ]         -- ignore other terms.

> tmp_cmd "Undo" (Name_S (Plain ('x':n)) : _)
>  | all isDigit n
>     = Ok $ retrieveContext (read n)
> tmp_cmd "Undo" _
>  = Ok $ retrieveContext 1        -- 2 for this context and the previous one
>            -- NOT HAPPY - check offsets & explanation! 

> tmp_cmd "ReturnAll" []
>  = Ok $ returnAllIntros
> tmp_cmd "Return" []
>  = Ok $ returnIntros Nothing
> tmp_cmd "Return" [Name_S n] 
>  = Ok $ returnIntros (Just n)

> tmp_cmd "KillRef" []
>  = Ok $ killref
> tmp_cmd "Save" []
>  = Ok $ fake_save_proof


> tmp_cmd "rebuild" []
>  = Ok $ rebuildLookupTable



> tmp_cmd "NormalType" [Name_S n]
>  = Ok $ whnfOnType n                 

> tmp_cmd "Expand" ns
>  = Ok $ mapM_ (expand Nothing) [ n | Name_S n <- ns ]
> tmp_cmd "Lift" [Name_S def, Name_S tgt]
>  = Ok $ lift_over def tgt



> tmp_cmd "flags" _  
>  = Ok $ showOptionSettings 
> tmp_cmd "FLAGS" _  
>  = Ok $ showOptionInfo 

> tmp_cmd "SHOW_DEBUG" _
>  = Ok $ liftIO_M_ $ showDebugOptions
> tmp_cmd "DEBUG" ns
>  = Ok $ liftIO_M_ $ chgDebugOptions (++ [ n | Name_S (Plain n) <- ns ])
> tmp_cmd "NO_DEBUG" ns
>  = Ok $ liftIO_M_ $ chgDebugOptions (\\ [ n | Name_S (Plain n) <- ns ])


> tmp_cmd "show" (Name_S n :_)
>  = Ok $ use_M_ (Ok . showCtxt (Show_Detailed n)) >>= p_Normal
> tmp_cmd "showi" (Name_S n :_)
>  = Ok $ use_M_ (Ok . showCtxt (Show_Internal n)) >>= p_Normal

> tmp_cmd "showa" (st:sts)
>  = Ok $ do
>            let t = ap_ts st sts
>            c <- get_M_ 
>            (_,t1) <- simple_MetaVars (freeNamesOf c) t
>            (TT t2 _) <- use_M_ (\c -> inferType c t1)
>            print_M_ (pretty_crude_print (dwhnf t2) ++ "\n\n")


> tmp_cmd "PrfCtxt" _
>  = Ok $ use_M_ (Ok . showCtxt Show_MetaVars) >>= p_Normal

> tmp_cmd "CS" _
>  = Ok $ showCoercions

> tmp_cmd "RWC" (t:ns@(_:_))
>  = Ok 
>  $ do
>        original <- get_M_
>        let new_ctxt = restrictCoercions [ n | Name_S (Plain n) <- ns ] original
>        set_M_ new_ctxt
>        do_cmd (Tactic (Basic (Refine Nothing t)))
>        set_M_ original
>    -- refine with specific coercions
>    -- works on most recent metavar.


> tmp_cmd "Init" _
>  = Ok $ clearContexts

---
Various Reductions
 ehnf = ihnf on names which might be definitions
 ihnf = iota reduction, with iotas on defs that are iota-ble.
 u_hnf = hnf on definitions 
 others = obvious

> tmp_cmd "edwhnf" ts@(_:_)
>  = Ok $ do_term (Just $ return . reduceAppliedAbstrs names)  term
>    where
>    (names,term) = case span is_name ts of
>                     (ns,[]) -> ( [ n | Name_S n <- init ns]
>                                , last ts)
>                     (ns,ts) -> ( [ n | Name_S n <-      ns]
>                                , ap_ts (head ts) (tail ts))
>    is_name (Name_S _) = True
>    is_name _          = False
> tmp_cmd "whnf" (t:ts)
>  = Ok $ do_term (Just $ return . whnf)  (ap_ts t ts)
> tmp_cmd "dwhnf" (t:ts)
>  = Ok $ do_term (Just $ return . dwhnf) (ap_ts t ts)
> tmp_cmd "ehnf" (t:ts)
>  = Ok $ do_term (Just $ \t -> use_M_ (\c -> Ok $ ihnf c (dwhnf t)))
>                 (ap_ts t ts)
> tmp_cmd "ihnf" (t:ts)
>  = Ok $ do_term (Just $ \t -> use_M_ (\c -> Ok $ ihnf c t)) (ap_ts t ts)

> tmp_cmd "u_hnf" ns
>  = Ok $ 
>    catch_M_ (\s -> fail_with_msg $ "Problem in u_hnf: " ++ s) $ 
>    do
>        mapM_ update_then_show_def [ n | n@(Name_S _) <- ns ]
>    where
>        update_then_show_def t@(Name_S n)
>         = do 
>            rt <- use_M_ $ \c -> translate_db c t >>= \t -> return (hnf c t)
>            print_M_ $ "Updating def. " ++ show n ++ " with reduced form.\n" 
>            rt `fseq` chg_M_ $ update_def_in_Ctxt n rt
>            print_M_ $ "Updated\n"
>            c_update <- get_M_ 
>            -- do_term return t         -- very long to show. 
>            set_M_ c_update


---

> tmp_cmd "ECHO" ws
>  = Ok $ print_M_ (unwords (map show ws) ++ "\n") 
> tmp_cmd "TRACE" ws
>  = Ok $ trace ("TRACE_CMD " ++ unwords (map show ws) ++ "\n") $ return ()

---
crude timing mechanism
 - best to reset timer when finished? 

> tmp_cmd "START" ws
>  = Ok $ liftIO_M_ $ 
>    do 
>        p <- getProcessTimes
>        writeIORef last_time p
>        putStrLn $ "Starting timer."

> tmp_cmd "TCHECK" ws
>  = tmp_cmd "FINISH" ws        -- checkpoint, currently same as Finish 
>                                -- since nothing happens to the timer. 

> tmp_cmd "FINISH" ws
>  = Ok $ liftIO_M_ $ 
>    do 
>        putStrLn $ "Time at FINISH command:"
>        p1 <- readIORef last_time 
>        reportTimes p1

---

> tmp_cmd "I" [t]
>  = Ok $ 
>    do
>        t'  <- use_M_ $ \c -> translate_db c t
>        use_M_ (\c -> Ok $ "TERM = " ++ showTerm_ c t' ++ "\n") >>= print_M_ 
>        use_M_ (\c -> test_CTC c t' Nothing) >>= print_M_ 
>        print_M_ ""

> tmp_cmd "I" [t, ty]
>  = Ok $ 
>    do
>        t'  <- use_M_ $ \c -> translate_db c t
>        ty' <- use_M_ $ \c -> translate_db c ty
>        use_M_ (\c -> Ok $ "TERM = " ++ showTerm_ c t' ++ "\n") >>= print_M_ 
>        use_M_ (\c -> Ok $ "TYPE = " ++ showTerm_ c ty' ++ "\n") >>= print_M_ 
>        use_M_ (\c -> test_CTC c t' (Just ty')) >>= print_M_ 
>        print_M_ ""

---

> tmp_cmd "dump" _
>  = Ok $ 
>    do
>	c <- get_M_
>	liftIO_M_ $ dump_modules c

---
Catch-all case
 - we couldn't interpret this as a command.

> tmp_cmd c as 
>  = fail_with_msg $ "Not a temporary cmd: " ++ c

---
Used in timing mechanism

#ifdef __GLASGOW_HASKELL__

> last_time :: IORef ProcessTimes
> last_time = unsafePerformIO (getProcessTimes >>= newIORef)
> {-# NoInline last_time #-}

> reportTimes :: ProcessTimes -> IO ()
> reportTimes p1 
>  = do
>        p2 <- getProcessTimes
>        ticks <- getSysVar ClockTick
>        let show_times tf = show ( fromIntegral (tf p2 - tf p1) 
>                                 / fromIntegral ticks)
>        putStrLn $ "\n"
>        putStrLn $ "-- U Time = " ++ show_times userTime 
>        putStrLn $ "-- E Time = " ++ show_times elapsedTime 

#else

> type ProcessTimes = Int
> last_time :: IORef ProcessTimes
> last_time = unsafePerformIO (newIORef 0)
> {-# NoInline last_time #-}
> reportTimes :: ProcessTimes -> IO ()
> reportTimes _ 
>  = putStrLn "Timings not implemented in Hugs."
> getProcessTimes :: IO ProcessTimes
> getProcessTimes = return 0

#endif 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Aux fns for handling terms by themselves

---
`catch_error_calls'
  - catches calls to "error".
  - a no-op for hugs.

> catch_error_calls :: IM -> IM

#ifdef __GLASGOW_HASKELL__
> catch_error_calls (M_ im)
>  = M_ 
>  $ \s -> do
>             r <- tryIO justErrors (im s)
>             case r of 
>               Left e   -> return (Fail $ "CAUGHT: " ++ show e)
>               Right ok -> return ok
#else 
> catch_error_calls im = im
#endif


---
`do_term' essentially shows or reduces a term, depending on the flag.

> type TermReductionFn = Maybe (Term -> CM Term)

> do_term :: TermReductionFn -> SimpleTerm -> IM
> do_term reduce_fn t
>  = do
>        c_orig <- get_M_ 
>        (m_ctxt, t1) <- simple_MetaVars (freeNamesOf c_orig) t
>        let chg_s = maybe "" (\cs -> "Ctxt Changes:\n" ++ cs ++ "\n") m_ctxt
>        p_Normal chg_s

>        --tx <- use_M_ $ \c -> Ok $ "DoTerm " ++ showTerm_ c t1 ++ "\n"
>        --print_M_ tx
>        --print_M_ $ "DoTerm " ++ show t1 ++ "\n"

>        (TT t2 ty_t1) <- use_M_ (\c -> inferType c t1)

>        c <- get_M_ 
>        case reduce_fn of
>            Just fn -> fn t2 >>= show_TermWithType c 
>            Nothing -> do
>                        let has_nm = nameFromTerm t2
>                        let show_opt = Show_Detailed . fromJust 
>                        if isNothing has_nm
>                            then show_TermWithType c t2
>                            else p_Normal (showCtxt (show_opt has_nm) c)
>        p_Normal "\n"    -- maybe remove, to reduce whitespace?
>        set_M_ c_orig


---
`do_conv' - test function for convertibility, with option of specifying
local context first (it forgets this context after the test is done). 

NB the terms go through the type checker first, to ensure that they have 
valid types, and also to insert coercions etc.

NB NB - in fact, this op should produce something tagged as LF_Term, which 
signifies that it is legal to use in conversion etc (TO BE DONE).

> do_conv :: ConvOpt -> [Def_Decl] -> SimpleTerm -> SimpleTerm -> IM
> do_conv opt ds l r
>  = prepend_msg "Conversion test" $
>    do
>        c_orig <- get_M_

<>        (TT l' _) <- use_M_ (\c -> translate_db c l >>= inferType c)
<>        (TT r' _) <- use_M_ (\c -> translate_db c r >>= inferType c)

>        (ccs, cvt) <- contextChanges $
>                        do 
>                            do_cmd (Defs_Decls ds)
>                            (l', _) <- def_mvs l
>                            (r', _) <- def_mvs r
>                            (TT l'' ty_l') <- use_M_ $ \c -> inferType c l'
>                            (TT r'' ty_r') <- use_M_ $ \c -> inferType c r'
>                            ty_ok <- use_M_ $ \c -> 
>                                     test_conv_or_unify opt c ty_l' ty_r'
>                            print_M_ $ "TYPES: " ++ ty_ok ++ "\n"
>                            use_M_ (\c -> test_conv_or_unify opt c l'' r'')
>        case ccs of 
>            Nothing -> return ()
>            Just x  -> print_M_ $ showCtxtDiffs c_orig x ++ "\n"
>        print_M_ $ cvt ++ "\n"

>        set_M_ c_orig                -- return to original context.
>    where
>        def_mvs t 
>         = get_M_ >>= \c -> definition_MetaVars (freeNamesOf c) t Nothing


---
`show_TermWithType' infers type of a term, and shows them nicely if the type
is ok. Takes explicit context, since it is used in cmds that produce extended
contexts and then discard them.

> show_TermWithType :: Ctxt -> Term -> IM
> show_TermWithType c t 
>  = do
>        (TT t2 ty) <- embed $ inferType c t
>        p_Normal $ showTermAndType c t2 ty

