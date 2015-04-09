> module Main (main) where

> import Data.Char(chr, isSpace)
> import System.IO
> import Plastic

#ifdef __GLASGOW_HASKELL__ 
<> import IOExts_
#endif

%-------------------------------------------------------------------------------

> main 
>  = do
#ifdef __GLASGOW_HASKELL__
>		hSetBuffering stdin  LineBuffering -- NoBuffering
>		hSetBuffering stdout LineBuffering -- NoBuffering
>		-- hSetBuffering stderr LineBuffering -- NoBuffering -- a problem?
>		stderr `seq` return ()	-- 4.04 fix for blocked traces
#endif

>		putStr $ replicate 80 '\\' ++ "\n"
>		putStr $ unlines header 
>		putStr $ "\n" ++ replicate 80 '-' ++ "\n\n"
>		print_build_info
>		putStr "\n"
>		putStr $ replicate 80 '/' ++ "\n"
>		putStr "\n\n"
>		top_loop


%-------------------------------------------------------------------------------

> header 
>  = [
>		"Plastic - a proof assistant for " ++ lf
>		, ""
>		, "by: Paul Callaghan, 1998-2001" 
>		, "Computer Assisted Reasoning Group, www.dur.ac.uk/CARG"
>		, "Dept of Computer Science, University of Durham" 
>		, ""
>		, "Version = " ++ "(not given)"
>		]

> lf = "Luo's typed version of Martin-L" ++ [chr 246] ++ "f's LF"

%-------------------------------------------------------------------------------

> print_build_info :: IO ()
#undef __GLASGOW_HASKELL__ 
#if __GLASGOW_HASKELL__ 
> print_build_info = _ccall_ print_info 
#else 
> print_build_info = putStrLn "\t{no build info with Hugs}"
#endif


