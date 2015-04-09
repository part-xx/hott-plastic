> module TestParser where


> import List(isPrefixOf)

> import Base(OkF(..))
> import LF_Parser(parseThenCall)
> import CommandTypes


%-------------------------------------------------------------------------------

> testTables
>  = [(("<",">"), Just "-%"), (("{","}"), Just "=*")]



%-------------------------------------------------------------------------------
old stuff, for vt100 pagination

> -- main = method (pag 20)
> -- qmain = method (putStr . unlines)

<> method show_it
<>  = do
<>		ls <- exprs >>= preprocess 
<>		show_it (fmap concat $ mapM process ls)



%-------------------------------------------------------------------------------

> preprocess :: [String] -> IO [String]
> preprocess = return . filter (not.null) . map one_line 

> one_line ('>':' ':'&':'p':' ':s) = s
> one_line s@('@':_) = s
> one_line s = ""

> m2 
>  = sequence $
>	 repeat $ 
>	 do
>		l <- getLine'
>		if l == "xx" then error "finished" else return ()
>		process l `catch` \e -> putStr_ 78 ("ERROR: " ++ show e ++ "\n\n")

> process :: String -> IO ()
> process cs@('@':_)
>  = putStr_ 78 (unlines ["", cs, ""])	-- external comment
> process cs
>  = do
>		r <- parseThenCall do_cmd do_term testTables $ takeWhile (/='#') cs
>		putStr_ 78 (unlines [cs, "\t" ++ r])
>    where
>		do_term = return . show_term
>		do_cmd  = return . show


%-------------------------------------------------------------------------------

> show_term :: Show a => a -> String
> show_term = simplify . show

> simplify :: String -> String
> simplify [] = []
> simplify s@(c:cs)
>  | "Ap_S " `isPrefixOf` s 
>     = simplify $ drop 5 s
>  | "(Name_S " `isPrefixOf` s 
>     = let (h,t) = span (/=')') $ drop 8 s 
>       in h ++ simplify (drop 1 t)
>  | "Nothing " `isPrefixOf` s 
>     = "_" ++ simplify (drop 7 s)
>  | "(Just \"" `isPrefixOf` s 
>     = let (h,t) = span (/=')') $ drop 6 s 
>       in h ++ simplify (drop 1 t)
>  | "(Just " `isPrefixOf` s 
>     = "(" ++ simplify (drop 6 s)
>  | otherwise 
>     = c : simplify cs



%-------------------------------------------------------------------------------


> exprs :: IO [String]
> exprs
>  = do
>		ls <- readFile "../test/parsing/exprs"
>		return $ filter not_blank $ lines ls

> not_blank []      = False		-- blank line
> not_blank ('#':_) = False		-- internal comment
> not_blank _       = True


%-------------------------------------------------------------------------------

> getLine'   :: IO String
> getLine'    
>  = getChar >>= \c -> 
>    if c=='\n' then return ""
>    else (getLine' >>= \cs -> return (c:cs))

> pag :: Int -> [String] -> IO()
> pag n = sequence_ . map (show_and_query n) . split n

> show_and_query n s
>  = do
>		putStr (unlines s)
>		if (length s == n) then getChar else return ' '
>		return ()


> unfold h p t = map h . takeWhile p . iterate t
> split n = unfold (take n) (not.null) (drop n)

> putStr_ n = putStr . unlines . split n 

%---------------------------------------

tests for pal+ prototype

> tests 
>  = [
>		"[f = a]"
>		,"[f [] = a]"
>		,"[f = a]b"
>		,"[f [] = a]b"
>		,"[f [x:A] = a]b"
>		,"[f [x,y:A] = a]b"
>		,"[f [x,y:A,z:B c] = a]b"
>		,""
>		,""
>		]

> foo = mapM process $ filter (not.null) tests
