
> import List ((\\), nub)
> import DaVinciInterface
> import BaseFunctions(isSubstringOf)

Sample "application" for the attached Haskell types. 

Dependency graph generator:

Extracts dependency information from a makefile, and writes the dependency info
as a piece of daVinci graph. 

> main = main2 process 

> main2 f 
>  = do
>		m <- readFile "Makefile" 
>		writeFile "Makefile.gra" (f m)

> process
>  = show . 
>    map show_deps  . 
>    remove_unwanted .
>	 group_deps     .
>	 get_deps_lines

> get_deps_lines
>  = map dependencyToGraphNode .
>    filter    (not . prefixMatch "# DO NOT DELETE") .
>    dropWhile (not . prefixMatch "# DO NOT DELETE") .
>    filter (not.null) . 
>    lines


----

> remove_unwanted ds = foldr remove_by_pred ds predicates

> predicates :: [String -> Bool]
> predicates 
>  = [
>		-- (/="Plastic"), 
>		(/="GlobalOptions"), 
>		(/="CommandTypes"), 
>		(not . prefixMatch "Base"), 
>		(not . prefixMatch "Debug"),
>		(/="DependencyGraph"),
>		(/="DummyDependency")
>	]

> remove_by_pred 
>  :: (String -> Bool) -> [(String,[String])] -> [(String,[String])]
> remove_by_pred p
>  = map (\(f,ds) -> (f, filter p ds)) . filter (\(f,_) -> p f)

---

> show_deps :: (String,[String]) -> Graphterm
> show_deps (file,deps) 
>  = NL file $ 
>    N "" [hidden, text file, box_t] $
>    [ E "" [] $ R dep | dep <- deps \\ [file] ]	

> group_deps ss
>  = [ (n, nub $ concat $ map snd $ filter (\(f,_) -> f == n) ss) 
>													| n <- nub $ map fst ss ]

---
`dependencyToDepList' parses a single dependency line

ASSUMES single dep per line, as per ghc's makedepend

> dependencyToGraphNode :: String -> (String, [String])
> dependencyToGraphNode s
>  | False                             = error $ "bad line: |" ++ s ++ "|"
>  | ".lhs" `isSubstringOf` dep_string = (file, [])
>  | otherwise                         = (file, deps \\ [file])
>  where
>	(full_file,colon:dep_string) = break (==':') s
>	(file:deps) = map stripSuffix $ full_file : words dep_string

---
`stripSuffix' removes everything after (and including) the final '.' in a
string.

> stripSuffix 
>  = dropWhile (not.isAlpha) . reverse . drop 1 . dropWhile (/='.') . reverse


---
`prefixMatch' tests for its first arg being the initial part of its second arg.
Eg (prefixMatch [1,2,3] [1..] => True)

> prefixMatch :: Eq a => [a] -> [a] -> Bool
> prefixMatch []  _ = True
> prefixMatch  _ [] = False
> prefixMatch (p:ps) (s:ss) = p == s && prefixMatch ps ss




