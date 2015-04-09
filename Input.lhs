> {-# LANGUAGE ScopedTypeVariables #-}
> module Input (
>			processSemicolonLine
>			, preprocessString
>			, Input(..)
>			, I_File_Op(..)
>		) where

Segmenting input etc


#ifdef __HASKELL98__
#define fail ioError			{- global, be lazy -}
#else 
#define isAlphaNum isAlphanum
#endif

> import System.IO(stdout, hFlush)
> import System.IO.Error(ioeGetErrorString, ioeGetHandle, isEOFError)
> import System.IO(stdin)
#ifdef __GLASGOW_HASKELL__
> import System.IO(isEOF)
#endif
> import IOExts_ (unsafePerformIO)
> import System.Environment (getEnv)
> import System.Process (system)
> import Data.Char (isSpace, isDigit, isAlphaNum)
> import Data.List (groupBy, isPrefixOf, (\\), nub, sortBy)
> import Data.Maybe (fromJust, isJust)

> import Base
> import GlobalOptions(proofGeneralMode)

> import Control.Exception



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Preprocessed input is 3 kinds 
	- a single line comment, or 
	- some form of command which was spread over several lines.
	- or a file operation.

Preprocessing converts a string of (stdin) input to a list of such values. 
File imports are treated as textual inclusion, or a null operation if the 
file has not already been read. Nested dependencies are flattened, so that 
all imported files are processed before the main file. 

> data Input
>  = I_Cmt String
>  | I_Cmd !String [String]			-- strict cmd string, to reduce heap reqs.
>  | I_File FilePath I_File_Op
>  deriving Show

I_Cmd returns a command in a single line, inserting ( ) to reproduce binding
of layout, plus contains the original form as a list of strings for display.

(NB doesn't add ( ) yet - need notion of nested layout for this!)

I_Cmt contains a comment to be echoed, such as info about test cases.

I_File relates to file handling. Each file read has a full name, which is 
used for identification. The _Op flag indicates the actual file operation.

> data I_File_Op
>  = I_File_Start		-- at the beginning of a file
>  | I_File_End			-- after a file 
>  | I_File_Present		-- where "import" names a file previously read.
>  deriving Show

NOTE: 
 - imports are allowed throughout a file, and the import is expanded in place
   if not already done.
 - this lax approach allows finer control of context... (MAYBE)


---


%---------------------------------------
Keep list of loaded files and files being currently read 
 - both needed to avoid repeat imports and cycles.

> type State = ([FilePath], [FilePath])
> type IM = M_ State [Input]

> mk_State :: [FilePath] -> State
> mk_State fs = (fs,[])

> is_file_present f = use_M_ (\(ps,  _) -> Ok $ f `elem` ps)
> is_file_active f  = use_M_ (\(_ , as) -> Ok $ f `elem` as)
> active_files      = use_M_ (\(_ , as) -> Ok $ as)
> start_import f    = chg_M_ (\(ps, as) -> Ok $ (ps,     f : as))
> finish_import f   = chg_M_ (\(ps, as) -> Ok $ (f : ps, as \\ [f]))

> indent_print :: String -> M_ State ()
> indent_print s
>  = do
>		i <- use_M_ (\(_, as) -> Ok $ length as)
>		print_M_ $ replicate (i+i) ' ' ++ s 

%---------------------------------------
`read_and_process'
  - reads a file and processes it 
  - checks for file present or file being read at present
  - latter is caused by cycles in imports.

> read_and_process :: FileType -> String -> IM
> read_and_process t n
>  = do
>		indent_print $ "Trying to " ++ action t ++ " file: " ++ n ++ " ... " 
>		(full_nm, abbrev_nm) <- liftIO_M_ $ find_typed_file t n
>		print_M_ $ "(found)\n"

>		present <- is_file_present abbrev_nm
>		active  <- is_file_active  abbrev_nm
>		if present
>		  then do
>				 indent_print $ "File " ++ abbrev_nm ++ " already present.\n"
>				 return [I_File abbrev_nm I_File_Present]
>		  else if active 
>				 then do
>						as <- active_files
>						fail_with_msg $ "Import Cycle detected: " ++ show as
>				 else do
>						start_import abbrev_nm
>						indent_print $ "Reading: " ++ abbrev_nm ++ "\n"
>						text <- readFile_with_catch full_nm
>						-- print_M_ $ "Read: " ++ n ++ " " ++ show (length text) ++ "\n" ++ unlines ["\t" ++ l | l <- lines text ]
>						is <- preprocess_file text
>						indent_print $ "Finished reading: " ++ abbrev_nm ++ "\n"
>						finish_import abbrev_nm
>						return $ I_File abbrev_nm I_File_Start 
>							    	: is
>									++ [I_File abbrev_nm I_File_End]



%-------------------------------------------------------------------------------
Decode text from a file.
 - need to redo these comments.


`literate_prefix'
  - the char which preceeds each line in literate mode

> literate_prefix :: Char
> literate_prefix  = '>'		-- nb several direct occs in patterns.

> line_separator :: Char
> line_separator  = ';'

---

> preprocessString :: [FilePath] -> String -> M_ s [Input]
> preprocessString fs 
>  = embed_other_M_ (mk_State fs) . preprocess_file

---

> preprocess_file :: String -> IM
> preprocess_file s 
>  | take 1 (dropWhile isSpace s2) == [literate_prefix]
>     = preprocess_literate_file s2
>  | otherwise 
>     = preprocess_plain_file s2
>  where
>		s2 = unlines $ takeWhile (not . (end_string `isPrefixOf`)) $ lines s
>		end_string = "__END__"

---
- remove nested comments (* *) style
- split by semicolons
- comments are just REMOVED - no attempt to encode them as Input values.
	(Q: any bad consequences?)

> preprocess_plain_file s
>  = case remove_nested_comments s of
>		Fail m -> fail_with_msg m
>		Ok  s' -> fmap concat
>					$ mapM expand_request
>					$ map (\c -> let ls = filter (not.null) (lines c)
>					                 in I_Cmd (unwords ls) ls)
>					$ map (dropWhile isSpace)
>					$ filter (/=[line_separator])
>					$ drop_trailing_cmd
>					$ splitBy (==line_separator) 
>					$ s'
>    where
>		drop_trailing_cmd [] = []
>		drop_trailing_cmd ls = init ls
>		-- This removes any non-semicolon'd command, or if there is none,
>		-- then removes trailing white space or the final semicolon.
>		-- This is to prevent non-semicolon'd statements appearing as cmds.

> preprocess_literate_file
>  = fmap concat 
>    . mapM expand_request 
>    . map remove_trailing_semicolons
>    . segment 0 ("",[])
>    . filter (\l -> not (null l) && head l == literate_prefix)
>    . lines

---
`remove_nested_comments'
  - remove lego-style comments from string
  - comments may nest.

> remove_nested_comments :: String -> OkF String
> remove_nested_comments = remove_nested_comments_ 0 

> remove_nested_comments_ n ('(':'*':cs)
>  = remove_comment_tail (n+1) cs
> remove_nested_comments_ n (c:cs)
>  = fmap (c :) $ remove_nested_comments_ n cs
> remove_nested_comments_ n [] 
>  | n == 0    = Ok []
>  | otherwise = Fail $ "End of string at comment depth " ++ show n

> remove_comment_tail n ('*':')':cs)
>  | n == 0    = Fail $ "Problem with comment nesting, rest = " ++ cs
>  | n == 1    = remove_nested_comments_ 0 cs	-- back to top
>  | otherwise = remove_comment_tail (n-1) cs	-- up a level

> remove_comment_tail n ('(':'*':cs)
>  = remove_comment_tail (n+1) cs				-- new comment
> remove_comment_tail n (c:cs)
>  = remove_comment_tail n cs
> remove_comment_tail n [] 
>  = Fail $ "End of string at comment depth " ++ show n


---
`remove_trailing_semicolons'
  - some files produced in emacs interface may have ; at the end of commands.
  - so remove them.

> remove_trailing_semicolons :: Input -> Input
> remove_trailing_semicolons (I_Cmd c ss) = I_Cmd (stripped c) ss
> remove_trailing_semicolons i            = i
> stripped = reverse . dropWhile (==';') . dropWhile isSpace . reverse


---
looks for import statements.

> expand_request :: Input -> IM
> expand_request i@(I_Cmd c ss)
>  | all isSpace c          = return [] 
>  | first_word == "import" = do_import Import rest
>  | take 1 c == "$"        = do_import Import (words $ drop 1 c)
>  | first_word == "test"   = do_import Test   rest
>  | take 1 c == "#"        = do_import Test   (words $ drop 1 c)
>  | otherwise              = return [i]
>  where
>		(first_word:rest) = words c
>		do_import i = fmap concat . mapM (read_and_process i) 

> expand_request i = return [i]


---
rather crude impl. of layout and literate scripts.

> segment _ (cmd,ss) []
>  = [I_Cmd cmd ss]

> segment i (cmd,ss) (('>':cs@(c:_)):ls)
>  | not $ isSpace c = segment i (cmd,ss) (('>':' ':cs):ls)

> segment i (cmd,ss) (('>':ws:'@':cs):ls)
>  | isSpace ws 
>     = I_Cmt (dropWhile isSpace cs) : segment i (cmd,ss) ls

> segment i (cmd,ss) (('>':ws:[]):ls)     
>  | isSpace ws 
>     = segment i (cmd,ss) ls

>	-- NB funny case for initial space vs tab...
>	-- basically, can't indent with single space, but can with single tab.
> segment i (cmd,ss) (('>':ws:cs@(c:_)):ls)  
>  | ws == ' ' && not (isSpace c) && not (null cmd)
>     = I_Cmd cmd ss : segment 0 (strip_cmt cs,[cs]) ls
>  | isSpace ws 
>     = segment indentation (cmd ++ strip_cmt cs, ss++[ws:cs]) ls
>  where
>		indentation = length $ takeWhile isSpace cs

> segment i (cmd,ss) (l@('@':_):ls) = segment i (cmd,ss) (("> " ++ l) : ls)
> segment i (cmd,ss) (l:ls) = segment i (cmd,ss) ls

---
`strip_cmt' 
  - ignore rest of line after "--" if it is followed by a space or text
  - NB:  this allows "----" to act like "--" followed by comment.
	BUT: unlikely to occur... 

> strip_cmt []                 = ""
> strip_cmt ('-':'-':[])       = ""
> strip_cmt ('-':'-':c:cs) 
>  | isAlphaNum c || isSpace c = ""
>  | otherwise                 = "--" ++ strip_cmt (c:cs) 
> strip_cmt (c:cs)             = c : strip_cmt cs





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Stdin String Processing
  - collect string until we see a semicolon, printing prompts appropriately.
  - only return with sequence ";\n" - thus collecting several lines if needed.
  - behaviour of prompt-printing could confuse some people - it is "eager" (!)
  - so might revert to single line basis...


IMPORTANT * IMPORTANT * IMPORTANT * IMPORTANT * IMPORTANT *

assume LINE-buffering - which is what ghc does by default. 
	(hugs doesn't - it uses no buffering.)

proof general (interactive) mode requires one prompt per line, else the error
stuff gets confused. 

might be an idea to adopt this everywhere? 

IMPORTANT * IMPORTANT * IMPORTANT * IMPORTANT * IMPORTANT *

---
MOVE UP TOP LATER.
`processSemicolonLine'
  - process a top-level string, by making it look like a line from a file.
  - code as embeddable in the outer monad, without knowing what it is.
  - REDO

> processSemicolonLine :: [FilePath] -> M_ s (Maybe [Input])
> processSemicolonLine fs
>  = do 
>		m_ss <- liftIO_M_ getSemicolonLine
>		case m_ss of 
>			Nothing -> return Nothing
>			Just ss -> fmap Just $ preprocessString fs $ unlines ss


---
Show prompts.
 - if using Proof General, then show the annotation char.

> print_prompt       
>  = do
>		-- hFlush stdout	-- so PG can see it
>		putStr "LF> "
>		b <- proofGeneralMode 
>		if b then putStr "\249\n" else return ()
>		hFlush stdout	-- so PG can see it

> print_under_prompt 
>  = do
>		putStr "> " 
>		hFlush stdout   -- so PG can see it


---

> getSemicolonLine :: IO (Maybe [String])
> getSemicolonLine 
>  = flip catch only_catch_eof
>  $ fmap Just
>  $ do
>		print_prompt
>		c <- while (return.isSpace) getChar
>		if (c == ';')
>			then return []
>			else do
>					cs <- get_semicolon_line_
>					return $ glue c cs
>    where
>		only_catch_eof e | isEOFError e = return Nothing
>		                 | otherwise    = ioError e

> get_semicolon_line_ 
>  = do
>		c <- getChar
>		case c of 
>			';'  -> do
>						return [";"]

<>						putStr "\n"
<>						-- putStr prompt		-- debate!
<>						c <- getChar
<>						if (c == '\n') 
<>							then putStr "\n" >> return [""]
<>							else do
<>									print_prompt			-- debate 2.
<>									cs <- get_semicolon_line_ 
<>									return ("" : glue c cs)

>			'\n' -> do
>						print_under_prompt
>						cs <- get_semicolon_line_
>						return $ "" : cs -- glue ' ' cs
>			_    -> do
>						cs <- get_semicolon_line_
>						return $ glue c cs

--
aux.

> glue c [] = [[c]]
> glue c (cs:css) = (c:cs):css


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Low-level File stuff.


`readFile_with_catch'

> readFile_with_catch :: FilePath -> M_ s String
> readFile_with_catch s
>  = liftOkF_IO_M_ $
>	 (readFile s >>= \i -> return (Ok i))
>    `catch` \e ->
>    return (Fail $ "Error on file (" ++ s ++ "): " ++ ioeGetErrorString e)



%-------------------------------------------------------------------------------
`find_typed_file'
  - selects appropriate places to look for files.
  - Import vs Test files are retrieved using appropriate paths (from env)
  - absolute pathnames (starting with /) are left intact.
  - the pair is full name, shortened name

> type FileNames = (String, String)

> data FileType = Test | Import

> action :: FileType -> String
> action Test   = "read test" 
> action Import = "import"

> find_typed_file :: FileType -> FilePath -> IO FileNames
> find_typed_file _ n@('/':_) = return (n,n)
> find_typed_file _ n@('.':_) = return (n,n)

> find_typed_file Import n 
>  = do
>	ps <- lib_paths
>	file <- find_file (".":ps) (n ++ ".o")
>	        `catch` \(e'::SomeException) -> find_file ps (n ++ ".lf")
>	                      `catch` \(e::SomeException) -> fail_with_msg (show e)
>	return (file, abbreviate_path_name "LIB" ps file)

> find_typed_file Test n 
>  = do
>		ps <- test_paths
>		file <- find_file ps n 
>		             `catch` \e1 -> find_file ps (n ++ ".lf")
>		             `catch` \e2 -> fail_with_msg (ioeToString e1 ++ "\n" ++ ioeToString e2)
>		return (file, abbreviate_path_name "TEST" ps file)

---
IDEA: (file) Extensions may need to change to identify different SpTT? 

<> add_extension = (++ ".lf")


---
Squeeze out long path names.

> abbreviate_path_name label ps s
>  = case (filter (`isPrefixOf` s) longest_first) of
>		p:_ -> "<" ++ label ++ ">" ++ drop (length p) s
>		_   -> s	-- show full name
>    where
>	longest_first = sortBy (\a b -> length b `compare` length a) ps


---

TODO - put to globalOptions
Careful with the defaults? 

> lib_env_var = "PLASTIC_LIB" 
> lib_default = "./lib"

> lib_paths
>  = do
>		s <- getEnv lib_env_var `catch` (\(e::SomeException) -> return lib_default)
>		return $ splitOn (==':') s

> test_env_var = "PLASTIC_TEST"
> test_default = "./test"

> test_paths
>  = do
>		s <- getEnv test_env_var `catch` (\(e::SomeException) -> return test_default)
>		return $ "." : splitOn (==':') s

Add current directory to test paths.

---
Locate file in list of directories.
 - No direct dir searching in hugs, so hack with unix find
 - success means copy the contents to a pre-arranged file
 - use lack of file to mean failure.

> t_file :: String
#if 1 
> t_file 
>  = unsafePerformIO (getEnv "HOME" `catch` \(e::SomeException) -> return ".") 
>		++ "/" ++ ".transfer-plastic"
#else 
> t_file 
>  = "/tmp/p_t." ++ show pid
>    where
>		pid = unsafePerformIO $ _ccall_ getpid :: Int
#endif

> find_file :: [String] -> String -> IO String
> find_file ps s
>  = do
>		system $ "rm -f " ++ t_file ++ " && "
>		         ++ "find " ++ find_args ++ " > " ++ t_file ++ " 2>/dev/null"

>		fs_ <- readFile t_file
>		case (nub $ lines fs_) of
>			[]         -> fail   $ userError $ "no file "++s++" in "++show ps
>			[f]        -> return $ f
>			ls@(_:_:_) -> fail   $ userError $ "Multiple files: " ++ show ls
>    where
>		find_args = unwords ps ++ " -name '" ++ s ++ "' -type f -follow -print"




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Test functions.

> test_input
>  = do
>		is <- fmap (fmap snd) $ run_M_ (processSemicolonLine []) ([],[])
>		let (Ok (Just (I_Cmd l _:_))) = is
>		if (l == "xx") then error "END" else return ()
>		elimOk (\m -> putStr $ "FAIL: " ++ m) 
>			   (\is -> putStr $ "OK:" ++ unlines (map show $ fromJust is))
>			   is
>		test_input
>		return ()		-- to ensure Hugs can run it.
