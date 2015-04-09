> module PrettyAux where

Some defintions built directly on top of PrettyPrinting lib.

OR replacements, for omitted functionality in the lib.


> import Data.List (transpose)

> import Pretty
> import FastString

%-------------------------------------------------------------------------------
Rendering Options

For use of prettyprinting lib,

`longRender' - putting things in nicely laid out format.
`shortRender' - effecively turning the pretttyprinting off. 



> shortRender = fullRender OneLineMode 80 1.0 string_txt "" 

> --longRender  = fullRender ZigZagMode  80 1.0 string_txt "" 
> longRender  = fullRender PageMode    80 1.0 string_txt "" 

> string_txt (Chr c)   s  = c:s
> string_txt (Str s1)  s2 = s1 ++ s2
> string_txt (PStr s1) s2 = (unpackFS s1) ++ s2
> string_txt (LStr s1 _) s2 = unpackLitString s1 ++ s2

---
Abbreviation for showing things.

> stext :: Show a => a -> Doc
> stext = text.show


%-------------------------------------------------------------------------------
TODO - play with these options!

ESP - ZigZagMode

%---------------------------------------

renderStyle  :: Style -> Doc -> String
data Style = Style { lineLength     :: Int,     -- In chars
                     ribbonsPerLine :: Float,   -- Ratio of ribbon length to
												line length
                     mode :: Mode
             }
style :: Style          -- The default style
style = Style { lineLength = 100, ribbonsPerLine = 2.5, mode = PageMode }
 
data Mode = PageMode            -- Normal
          | ZigZagMode          -- With zig-zag cuts
          | LeftMode            -- No indentation, infinitely long lines
          | OneLineMode         -- All on one line
 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Nice layout of tables of info.

Done by string hacking because Pretty lib can't do it (see below)

%---------------------------------------
V1 - basic string hacking

> tabulate :: [[String]] -> String
> tabulate ss
>  = "" ++ error "show_table NYI"
>    where
>		max_s = maximum $ map length $ concat ss
>		max_c = maximum $ map length ss
>		-- pad = map (map r_justify)
>		r_justify s = replicate (max_s - length s) ' ' ++ s

%---------------------------------------

THIS DOESN'T WORK - the lib doesn't do proper horizontal joining of boxes...
<> show_table :: [[String]] -> String
<> show_table = longRender . tabulate . map (map text)
<> tabulate :: [[Doc]] -> Doc
<> tabulate = hsep . map vcat . transpose
<> t1 = putStr $ show_table $ map (map show) [[1..5],[1..5],[1..5]]

%---------------------------------------
Second try

take documents and fixed line length, render each Doc in a small space, and 
glue these big boxes together as a big piece of text.

WANT: variable spacing, to allow good fit.

QQ: which side for justification?  

|----------|----------|
 xxxxxxxxxxxxxx xxxxx    GOOD

 xxxxxxxxx  xxxxx		 BAD
  xxxxxx

> horizontal_join :: Int -> [Doc] -> Doc
> horizontal_join = error "horizontal_join NYI"

