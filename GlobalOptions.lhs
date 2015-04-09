> module GlobalOptions (
>				setInitialOptions
>				, setOneOption
>				, showOptionSettings
>				, showOptionInfo

>				, prettyPrint_Options
>				, PP_Options(..)
>				, proofGeneralMode

>				, canOmitEl
>				, typeColonType
>				, allowEmptyTypes

>				, p_Quiet
>				, p_Occasional
>				, p_Normal
>				, p_Everything
>			) where

Handling of various cmdline and env var options, and access to these.

Wrapped up with the M_ monad (from StateMonads).

> import IOExts_ (unsafePerformIO)
> import IOExts_ (newIORef, readIORef, writeIORef, IORef)

--> import System (getArgs, getEnv)		-- NB posix also getEnvVar

> import System.Environment
> import Control.Monad (foldM)
> import Data.Maybe(isJust, fromJust)
> import Data.List (partition)
> import Data.Char(isSpace)

> import StateMonads
> import Base

> import PrettyAux(longRender)
> import Pretty(Doc, hsep, vcat, text, nest, (<+>), ($$))



%-------------------------------------------------------------------------------

NB newer versions of haskell have a GetOpt library.

However, it doesn't do the env var stuff, nor the flag-setting on the main
  options record. 

So we haven't lost much...


%-------------------------------------------------------------------------------

ideal, set once before starting the main IO, - eg on basis of cmdline_args? 
then, these things are read-only. 
QQ: IO_only interface for setting up? this would restrict.

%-------------------------------------------------------------------------------
VIEWS

subsets needed for different parts, and makes it reasonable to pass around in
small packages.

TODO: nest the strs inside the global str, and only export the inner parts.

Q: do I add the user syntax stuff to this? debatable... Partly, is context.


%-------------------------------------------------------------------------------

would like mechanism to set the options INTERNALLY and from the prompt, 
eg as a tmp cmd.

set X
set X opts

might need to have string as a valid id? could be fun(!)
eg "the small var" : A ...





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
LIST OF RECOGNISED OPTIONS
 - with their names, and their defaults, and ways to change them.

> options :: [Opt Options]
> options 
>  = [

GENERAL OPTIONS.

>	O "ALLOW_EMPTY_TYPES" 
>	  "allow-empty-types" 
>	  (show.allowEmptyTypes_)
>	  (F True 				$ \f v -> v {allowEmptyTypes_=f})
>	  (text "When False, forbids all declarations which may result"
>		$$ text "in empty types (for SFP)"),

>	O "TYPE_COLON_TYPE"
>	  "type-colon-type" 
>	  (show.typeColonType_)
>	  (F False 				$ \f v -> v {typeColonType_=f})
>	  (text "Adds Type:Type"),

>	O "CAN_OMIT_EL" 
>	  "can-omit-El" 
>	  (show.canOmitEl_)
>	  (F True 				$ \f v -> v {canOmitEl_=f})
>	  (text "User can omit El tags;"
>		$$ text "type synthesis will insert them where needed"),

>	O "VERBOSITY" 
>	  "verbosity" 
>	  (show.verbosityLevel_)
>	  (A (show Normal) 		$ chg_verbosity)
>	  (text "Select level of information passed back to user."),


PRETTY-PRINTING OPTIONS.

>	O "PROOF_GENERAL" 
>	  "proof-general" 
>	  (show.proofGeneral.pp_options)
>	  (F False  			(chg_pp $ \f v -> v {proofGeneral=f}))
>	  (text "Trigger any behaviours needed for Proof General"),

>	O "REDUCE_TERMS" 
>	  "reduce-terms" 
>	  (show.reduceTerms.pp_options)
>	  (F True 				(chg_pp $ \f v -> v {reduceTerms=f}))
>	  (text "When true, simplify term by reducing all redexes in it"),

>	O "SHOW_EL" 
>	  "show-El" 
>	  (show.showEl.pp_options)
>	  (F True 				(chg_pp $ \f v -> v {showEl=f}))
>	  (text "When false, then don't show El tags in pretty-printing."),

>	O "SHOW_CAST" 
>	  "show-CAST" 
>	  (show.showCAST.pp_options)
>	  (F False 				(chg_pp $ \f v -> v {showCAST=f}))
>	  (text "When false, don't show types in CASTs in pretty-printing."),

>	O "ARG_LAYOUT" 
>	  "arg-layout" 
>	  (show.argLayout.pp_options)
>	  (F True 				(chg_pp $ \f v -> v {argLayout=f}))
>	  (text "When True, use generous layout for application terms."),

>	O "SHOW_COERCIONS" 
>	  "show-coercions" 
>	  (show.showCoercions.pp_options)
>	  (F True 				(chg_pp $ \f v -> v {showCoercions=f}))
>	  (text "Show coercion components of terms"),

>	O "HIGHLIGHT_COERCIONS" 
>	  "highlight-coercions" 
>	  (show.highlightCoercions.pp_options)
>	  (F True				(chg_pp $ \f v -> v {highlightCoercions=f}))
>	  (text "If coercions are being shown, make them noticeable"),

>	O "MARK_CLOSED_TERMS" 
>	  "mark-closed-terms" 
>	  (show.markClosedTerms.pp_options)
>	  (F False				(chg_pp $ \f v -> v {markClosedTerms=f}))
>	  (text "If true, prefix closed terms with string <CT>; for debugging"),

>	O "SERVER_MODE" 
>	  "server-mode" 
>	  (show.serverMode.pp_options)
>	  (F False				(chg_pp $ \f v -> v {serverMode=f}))
>	  (text "If true, don't do usual sugarings") 

>    ]


---
Options are stored in this structure

> data Options
>  = Options {
>		pp_options			:: PP_Options,
>		allowEmptyTypes_    :: Bool,
>		canOmitEl_          :: Bool,
>		typeColonType_      :: Bool,
>		verbosityLevel_     :: Verbosity
>    } deriving (Show)


> data PP_Options
>  = PP_Options {
>	proofGeneral        :: Bool	-- determines "rendering"

>	,reduceTerms        :: Bool	-- show term with all possible redexes done

>	,showEl             :: Bool	-- syntax omissions
>	,showCAST           :: Bool

>	,argLayout          :: Bool	-- PP_ArgLayout	-- format.

>	,showCoercions      :: Bool	-- show term or not
>	,highlightCoercions :: Bool	-- highlight the term

>	,markClosedTerms    :: Bool	-- mark term if it is Closed (ie, fast)

>	,serverMode         :: Bool	-- don't do usual sugarings

>	-- other? 
>    } deriving (Show)


> data PP_ArgLayout = AL_None | AL_Spacious


> chg_pp :: (Bool -> PP_Options -> PP_Options) -> (Bool -> Options -> Options)
> chg_pp fn b v
>  = let ppo = pp_options v in
>    v {pp_options = fn b ppo} 

---
MOVE AWAY? 

> default_options = process_defaults options unset_options



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
EXTERNAL FUNCTIONS FOR OPTIONS
 - most should use just the field name of the option, and the generic get_f
   functions.

%---------------------------------------

> allowEmptyTypes :: M_ s Bool
> allowEmptyTypes = get_f allowEmptyTypes_

%---------------------------------------

> canOmitEl :: IO Bool
> canOmitEl = get_f_IO canOmitEl_

%---------------------------------------

> typeColonType :: IO Bool
> typeColonType = get_f_IO typeColonType_

%---------------------------------------
Verbosity levels
 - detail of print statements can be selected by here.
 - uses, eg for restricted printing in file read.

> data Verbosity 
>  = Quiet			-- bare minimum
>  | Occasional		-- might be of use
>  | Normal			-- all useful info
>  | Everything		-- all output
>    deriving (Show, Read, Eq, Ord)

> p_Quiet      = p_leq Quiet      :: String -> M_ s ()
> p_Occasional = p_leq Occasional :: String -> M_ s ()
> p_Normal     = p_leq Normal     :: String -> M_ s ()
> p_Everything = p_leq Everything :: String -> M_ s ()

> setVerbosity :: Verbosity -> M_ s ()
> setVerbosity v = liftOkF_IO_M_ $ setOneOption "verbosity" (Just $ show v)

> p_leq :: Verbosity -> String -> M_ s ()
> p_leq t s
>  = do
>		v <- get_f verbosityLevel_
>		if (t <= v) 
>			then print_M_ (s ++ "\n")
>			else return ()

> chg_verbosity :: String -> OkF (Options -> Options)
> chg_verbosity s 
>  = maybe 
>		(Fail $ "verbosity value " ++ show s ++ " not recognised.")
>		(\v -> Ok $ \fs -> fs {verbosityLevel_=v})
>		(safeRead s)


%---------------------------------------

> prettyPrint_Options :: IO PP_Options
> prettyPrint_Options = get_f_IO pp_options

> proofGeneralMode :: IO Bool
> proofGeneralMode = fmap proofGeneral prettyPrint_Options



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Generic Options Handling

The options are stored in an IORef, which is grabbed UNSAFELY at the start. 
(Must ensure that flag setting is forced before the top loop begins.)


> {-# NoInline flags_ref #-}
> flags_ref :: IORef Options
> flags_ref = unsafePerformIO $ newIORef default_options

> get_f :: (Options -> a) -> M_ s a
> get_f = liftIO_M_ . get_f_IO

> get_f_IO :: (Options -> a) -> IO a
> get_f_IO f = readIORef flags_ref >>= return . f

---
Not used yet - will be, when internal control added.

 - AS DECODE, single opt, via code below? 

> setOptions :: (Options -> Options) -> IO ()
> setOptions f 
>  = do
>		v <- readIORef flags_ref 
>		writeIORef flags_ref (f v)


---
`showOptionInfo'
  - show the vars/flags and a description.

> showOptionInfo :: M_ s ()
> showOptionInfo
>  = do
>		print_M_ (unlines $ map show_info options)
>		print_M_ "Note: cmd-line flags can be negated by -no-X\n"
>		print_M_ "      (this isn't implemented for env vars yet.\n"
>		print_M_ "\n"

> show_info (O e c _ _ d) 
>  = longRender $
>    vcat [
>    	text "Opt" <+> text c,
>		nest 3 $ text "info" <+> d,
>		nest 3 $ text "var " <+> text e,
>		nest 3 $ text "flag" <+> text c,
>		text "" ]


---
`showOptionSettings' 
  - show the current values.

> showOptionSettings = liftIO_M_ showOptionSettings_ :: M_ s ()
> showOptionSettings_ 
>  = do
>		putStr "Current Option settings\n"
>		fs <- readIORef flags_ref
>		putStr (show_fs fs)
>		putStr "\n\n"

> show_fs fs
>  = longRender $
>    nest 2 $
>    vcat [ text (pad c) <+> text (sf fs) | (O _ c sf _ _) <- options ]
>    where
>		max_nm = maximum [ length c | (O _ c _ _ _) <- options ]
>		pad s = take max_nm (s ++ repeat ' ')


%-------------------------------------------------------------------------------
Set flags on basis of any settings in cmdline or env var. 
Cmdline has priority.

DOES THIS BELONG HERE? 

> setInitialOptions :: IO ()
> setInitialOptions 
>  = do
>		as <- getArgs 
>		okfs <- mapM (do_opt $ separate_args as) options
>		let (oks, fails) = partition isOk okfs
>		sequence [ warn m | (Fail m) <- fails ]
>		warn $ if (null fails) then "" else "\n"

>		fs <- readIORef flags_ref
>		let fs' = foldr (.) id (map fromOk oks) $ fs
>		writeIORef flags_ref fs'
>		showOptionSettings_


%-------------------------------------------------------------------------------
`setOneOption'
  - sets option opt with the optional arg, 
  - name as per cmdline, but replacing "-" with "_", 
	eg "can-omit-El" -> "can_omit_El"

> setOneOption :: String -> Maybe String -> IO (OkF ())
> setOneOption opt arg
>  | null matches 
>     = return $ Fail $ "couldn't match option: " ++ (unwords $ concat opt2)
>  | isFail (head matches)
>     = return $ rethrow $ head matches
>  | otherwise
>     = do
>			putStr $ "Setting option: " ++ (unwords $ concat opt2) ++ "\n"
>			setOptions (fromOk $ head matches) 
>			return $ Ok ()
>  where   
>		matches = [ decode c t (fromJust b_as) 
>				   | (O _ c _ t _) <- options, 
>								let b_as = match opt2 c, isJust b_as]
>		opt2 = [ us_to_hyphen $ ("-" ++ opt) : maybe [] (:[]) arg ]
>		us_to_hyphen = map (map (\c -> if c == '_' then '-' else c))








%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Type for encoding how options are represented, and what we do with them.

TODO: add a nice help string? thus becoming self-documenting (self-reminding)! 

> type EnvVarName = String
> type CmdLineName = String
> type DocString = Doc

> data Opt a
>  = O EnvVarName CmdLineName (a -> String) (Opt_type a) DocString
>    deriving (Show)


---
`Opt_type'
  - have flags and argument-taking options
  - both have defaults, and a function to change the current value.
  - Args can yield arbitrary types via read on the string.

  - for F, if the option is used in the -no-X form, then the default is inverted

> data Opt_type a
>  = F Bool   (Bool ->        a -> a) 
>  | A String (String -> OkF (a -> a))
>    deriving (Show)


---
`process_defaults'
  - set the defaults as given in the specification
  - causes crash if there's a problem - since it is a developer-thing

> process_defaults :: [Opt a] -> a -> a
> process_defaults os a 
>  = elimOk (\m -> error ("process_defaults didn't succeed: " ++ m)) id $
>    mapM set os >>= \fs -> Ok (foldr id a fs)

> set :: Opt a -> OkF (a -> a)
> set (O _ _ _ (F d cf) _) = Ok $ cf d
> set (O _ _ _ (A d cf) _) = cf d

---

> showPossibleOptions :: Show a => [Opt a] -> String
> showPossibleOptions os
>  = "say what we expect... auto gen from options list...\n" 
>		++ unlines (map show os)
>		++ error "NYI!\n"

%---------------------------------------
Divide up the cmdline into sequences of an option (prefix "-") 
  and its args (until the next option).

> type Args = [String]
> separate_args :: [String] -> [Args]
> separate_args = segmentBy (\v -> take 1 v == "-")



%---------------------------------------
Now, the actual `discovery' of an option.
 - try the cmdline first
 - else try the environment
 - otherwise, return id as "no change required".

> do_opt :: [Args] -> Opt a -> IO (OkF (a -> a))
> do_opt as (O e c _ t _)
>  = do
>	match_cmd <- return $ match as c
>	case match_cmd of
>		Just b_as -> return $ decode c t b_as
>		Nothing   -> do
>				match_env <- lookup_in_env e
>				case match_env of
>					Just b_as -> return $ decode c t b_as
>					Nothing   -> return $ Ok id


---
`decode' 
  - apply change fn accordingly.
  - checking for "syntax"...

> decode :: String -> Opt_type a -> (Bool, [String]) -> OkF (a -> a)
> decode c (F d f) (b,[])  = Ok $ f b
> decode c (F d _) (b,as)  = Fail $ "Flag "++c++" unexpected args: " ++ show as
> decode c (A d f) (b,[a]) = f a
> decode c (A d f) (b,[])  = Fail $ "Flag "++c++" had no args, expected one"
> decode c (A d f) (b,as)  = Fail $ "Flag "++c++" had too many args: "++ show as


---


> lookup_in_env :: String -> IO (Maybe (Bool, [String]))
> lookup_in_env e 
>  = do
>		pos <- from_env $          e
>		neg <- from_env $ "NO_" ++ e
>		if isJust pos
>			then return $ Just (True, prepare $ fromJust pos)
>			else if isJust neg
>				 	then return $ Just (False, prepare $ fromJust neg)
>					else return Nothing
>    where
>		prepare "" = []		-- code up no-value as no args.
>		prepare s  = [s]	-- and an arg as singleton.


---
given the segmented cmdline arguments, find the match, if it exists.
the flag indicates whether option occurs in pos or neg sense.

> match :: [[String]] -> CmdLineName -> Maybe (Bool, [String])
> match [] c       = Nothing
> match ([]:ass) c = match ass c	-- empty arg, impossible
> match ((a:as):ass) c 
>  | drop 1 a == c          = Just (True,  as)
>  | drop 1 a == "no-" ++ c = Just (False, as)
>  | otherwise              = match ass c


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Minor Stuff.

Init (undef) value. Have to specify that each of the slots is undefined,
because the record update functions complain if a str. is totally undefined.

> unset_options :: Options
> unset_options
>  = Options pp_init unset unset unset unset 
>    where
>		unset = error "missing init in default_flags"
>		pp_init = PP_Options unset unset unset unset unset unset unset unset unset


