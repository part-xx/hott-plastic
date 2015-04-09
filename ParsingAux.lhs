> module ParsingAux 
>   (basic_lex, defs_decls_string, compileBrackets, Token(..), BracketInfo(..))
> where

Lexer and token defns.

#ifdef __HASKELL98__
#define isAlphanum isAlphaNum
#endif

> import Data.Maybe
> import Data.Char(isSpace, isDigit, isAlpha, isAlphanum) 
> import Data.List(sortBy, isPrefixOf, nub)

--#ifndef __GLASGOW_HASKELL__

> import Hugs_FiniteMap(listToFM, lookupFM, lookupWithDefaultFM)

--#else 
--> import FiniteMap(listToFM, lookupFM, lookupWithDefaultFM)
--#endif

> import Base
> import SharedSyntax(BracketPair(..), univ_separator)


%-------------------------------------------------------------------------------
Bracket Information
 - (all here, reduces import complexity)

 - indicates to the lexer which symbols are brackets.
 - in form of simple lookup table.

> type BracketInfo  = [(String,Token)]

---
Compilation of simple representation to something a bit better
 - SORTED IN DECREASING LENGTH FOR GREEDY MATCHING.

> compileBrackets :: [BracketPair] -> BracketInfo
> compileBrackets ses
>  = (if length symbols /= length (nub symbols) 
>     then warn_ ("Bracket symbols reused:\n" ++ unlines (map show result)) 
>     else id
>    ) (reverse sorted_list)
>  where
>		result = concat $ map split ses 
>		split se@(s,e) = [(s,TokenOpenBracket se),(e,TokenCloseBracket se)]
>		symbols = map fst result 
>
>		sorted_list  = sortBy length_ordering result
>		length_ordering a b = length (fst a) `compare` length (fst b)



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
The datastructure for the tokens, and a simple lexer...

NB: use as main lexer - eg add keywords arg to lex fn?

> defs_decls_string = "DDS"

> data Token
>	= TokenHitEOF
>	| TokenEOF
>	| TokenNL
>	| TokenFail String
>	| TokenNum  Int			-- unlikely to want Float? 

>	-- Command words
>	| TokenKeyword String		-- generic form
>	| TokenDefsDecls		-- this may replace next 2.
>	| TokenDecl
>	| TokenDefn

>	| TokenModule
>	| TokenCoercion
>	| TokenParameters
>	| TokenPrerequisites

>	| TokenBeginFunction	-- to delimit fn clauses in interactive mode.
>	| TokenEndFunction
>	| TokenWhere			-- aux defs for the above.

>	| TokenRefine
>	| TokenIntros
>	| TokenInduction
>	| TokenAssumption
>	| TokenImmed
>	| TokenEquiv
>	| TokenInductive
>	| TokenConstructors
>	| TokenWantInd
>	| TokenTheorems
>	| TokenRelation
>	| TokenRelation_LE
>	| TokenAlsoLift
>	| TokenNoDischarge
>	| TokenElimRule
>	| TokenSimpleElimRule
>	| TokenElse_T
>	| TokenThen_T
>	| TokenNormal
>	| TokenCtxt
>	| TokenJustParse
>	| TokenFiniteUniverse
>	| TokenInfiniteUniverse
>	| TokenCumulative
>	| TokenClosed
>	| TokenAddToUniverse


>	-- key words.
>	| TokenType
>	| TokenKind
>	| TokenEl
>	| TokenCase
>	| TokenOf
>	| TokenWith
>	| TokenSig
>	| TokenStr

>	-- identifiers
>	| TokenId String		-- for qualified, [String] String? 

Name SHOULD be ([String],String) if qualified throughout! 
NB it isn't the same as var.field - case that we should not overload names? 

TRY THIS: all dots are interp at top level parser
BUT WE CAN'T DO THIS for `id` yet! 
(TRY with a local def? which means fixity must be carried! !
   -- attaching to the id-in-package (NB different when let-ised/manifest)) 


>	| TokenBindId String    -- eg lambda[ ... ]

>	| TokenMeta String		-- eg ?var
>							-- need this distinct since TokenId is any id, and
>							-- we don't want bound vars containing [?] etc.

>	| TokenOpenBracket  (String,String)
>	| TokenCloseBracket (String,String)
>							-- store open/close pairs, so we can check balance

>	| TokenSymbol String	-- unrecognised char sequence

>	| TokenUniv String		-- eg Type^2  (see def of univ_separator)


>	-- multichar symbols
>	| TokenEqEq
>	| TokenColonColon		-- soft cast
>	| TokenColonExclColon	-- hard cast :!:

>	-- simple punctuation
>	| TokenEq
>	| TokenUnderSc
>	| TokenColon
>	| TokenComma
>	-- | TokenDot	-- special meaning removed
>	-- | TokenBar	-- special meaning removed
>	| TokenDollar
>	| TokenBacktick

>   -- bracketing punctuation
>	| TokenOP
>	| TokenCP
>	| TokenOB
>	| TokenCB
>	| TokenOA
>	| TokenCA
>	| TokenOC
>	| TokenCC
>	deriving Show



%---------------------------------------
a simple lexer that returns this datastructure plus unused string.

> basic_lex :: BracketInfo -> ST_ String Token
> basic_lex _ []        = pair [] TokenEOF
> basic_lex _ ('\n':cs) = pair cs TokenNL 
> basic_lex bi (c:cs)     
>  | isSpace c = basic_lex bi $ dropWhile isSpace cs

Below: metavariables case:
Allow these forms.
 - ?    = by itself
 - ?nnn = with some number
 - ?abc = with some valid id which is not a key word.

> basic_lex bi ('?':cs)
>  | null cs   = pair cs (mk_meta "")
>  | isDigit c = let (nn,rest) = span isDigit cs 
>                in pair rest (mk_meta nn)
>  | isAlpha c = let (rest,token) = lex_id cs
>                in case token of
>                     TokenId n -> pair rest (mk_meta n)
>                     other     -> pair cs   (mk_meta "") -- keyword case
>  | otherwise = pair cs (mk_meta "")
>  where
>	mk_meta s = TokenMeta $ '?' : s
>	c = head cs

> basic_lex _ cs@(c:_)   
>  | isDigit c
>     = pair rest (TokenNum $ read token)
>  where
>	(token,rest) = span isDigit cs

> basic_lex bi cs@(c:_)   
>  | isAlpha c 
>     = case token of
>			TokenEl      -> pair rest token		-- change? 
>			TokenId name ->
>				case rest of
>					[]     -> pair rest token
>					'[':xs -> pair xs (TokenBindId name) 
>					_:_    -> try_univ (TokenId name) name rest
>			TokenType -> try_univ TokenType "Type" rest
>			other     -> pair rest token 
>  where
>		(rest,token) = lex_id cs	-- NB rest starts with first non-id char

>		try_univ t0 name ('^':[]) 
>		 = pair [] (TokenUniv $ name ++ [univ_separator])
>		try_univ t0 name ('^':x:xs)
>		 | isSpace x = pair (x:xs) (TokenUniv $ name ++ [univ_separator])

>		try_univ t0 name ('^':xs)
>		 = case lex_id xs of
>				(ys, TokenId t)  
>					-> pair ys (TokenUniv $ name ++ univ_separator : t) 
>				(ys, TokenNum t) 
>					-> pair ys (TokenUniv $ name ++ univ_separator : show t) 
>				_ -> pair xs (TokenUniv $ name ++ [univ_separator])

>		try_univ t0 name xs = pair xs t0


%---------------------------------------
HERE
Infix `op` and sections.

bracketing -> specific Open/Close 
careful with settings above! 
special case for "(", since overloaded. 

*&)(&* -> "(" ++ string ++ ")"
"(&(&(*)" -> TokenSymbol, IF NOT A BRACKETING SYMBOL. 

If not caught, then default to original interp. 
	- this is more relevant for brackets! 


NB backticks handled in main parser.

> basic_lex bi cs
>  | not $ null matches_brackets = head matches_brackets
>  where
>	matches_brackets = bracket_match bi cs

---
Q: handling of single char symbols.

> basic_lex bi cs@(c:cs_)
>  | isJust single			-- NB these symbols can't start larger symbols
>     = pair cs_ (fromJust single)
>  | isJust symbol 
>     = fromJust symbol
>  where
>		single = single_lex c cs_
>		symbol = symbol_lex cs

> basic_lex bi cs			-- fail if we get here.
>  = pair cs (
>    TokenFail $ "didn't lex: " ++ cs
>    )


---

> symbol_lex :: STM String Maybe Token
> symbol_lex s 
>  = Just $ pair rest (TokenSymbol op)
>    where
>		(op,rest) = break ending s
>		ending c = isAlphanum c || isSpace c || c `elem` symbol_end_set

> symbol_end_set :: String
> symbol_end_set 
>  =    "(){}[]"   -- avoid confusion with brackets
>    ++ "'"        -- possible start of id. 
>    ++ "`"        -- backticks
>    ++ ""

---
Categorises single character.

IMPORTANT 
 - these CANNOT start a multi-char symbol. 
 - passes tail, can judge context

> single_lex :: Char -> String -> Maybe Token
> single_lex c cs
>  = case c of
>		'=' -> case cs of -- want to use in arrows, so special case
>					[] -> Just TokenEq	
>					d:ds | isSpace d || isAlphaNum d -> Just TokenEq
>					-- d:ds | isJust (foo d)            -> Just TokenEq
>					_  -> Nothing

>		c   -> foo c
>    where
>		foo c = case c of
> 			':' -> Just TokenColon
> 			'_' -> Just TokenUnderSc
> 			',' -> Just TokenComma
> 			'$' -> Just TokenDollar
> 			'`' -> Just TokenBacktick

> 			'.' -> Just $ TokenSymbol "."	-- acts like normal op now.
> 			'|' -> Just $ TokenSymbol "|"	-- ditto.

>			'[' -> Just TokenOB
>			']' -> Just TokenCB
> 			'(' -> Just TokenOP
>			')' -> Just TokenCP
>			'<' -> Just TokenOA
>			'>' -> Just TokenCA
>			'{' -> Just TokenOC
> 			'}' -> Just TokenCC
>			_   -> Nothing

%---------
MultiChar Symbols: greedy matching applies. 

---
BracketInfo assumed sorted by decreasing length.

> bracket_match :: BracketInfo -> String -> [(String, Token)]
> bracket_match ts cs
>  = [ (drop (length s) cs, t) | (s,t) <- ts, s `isPrefixOf` cs ]



%---------
Lexing for ids and keywords.

> lex_id [] 
>  = error "lex_id [] should not occur"
> lex_id (c:cs)
>  = (rest, lookup_keyword id)
>  where 
>		pred c = isAlphanum c || c == '_' || c == '\''
>		(id_tail,rest) = span pred cs
>		id = c : id_tail				-- NB assume (isAlpha c) true.

%---------
Keywords
 - might want to add in words from the SpTT ? 
 - (ignore possibility of duplications...)

> lookup_keyword :: String -> Token
> lookup_keyword id
>  = lookupWithDefaultFM keywords (TokenId id) id

> keywords = listToFM
>  [	
>	-- command word tokens.
>	(defs_decls_string, TokenDefsDecls),
>	("Decl", TokenDecl),
>	("Defn", TokenDefn),
>	("module", TokenModule),
>	("Coercion", TokenCoercion),
>	("Parameters", TokenParameters),
>	("Prerequisites", TokenPrerequisites),
>	("BeginF", TokenBeginFunction),
>	("EndF", TokenEndFunction),
>	("where", TokenWhere),
>	("Refine", TokenRefine),
>	("Intros", TokenIntros),
>	("Induction", TokenInduction),
>	("Equiv", TokenEquiv),
>	("Assumption", TokenAssumption),
>	("Immed", TokenImmed),
>	("Inductive", TokenInductive),
>	("Constructors", TokenConstructors),
>	("WantInd", TokenWantInd),
>	("Theorems", TokenTheorems),
>	("Relation", TokenRelation),
>	("Relation_LE", TokenRelation_LE),
>	("AlsoLift", TokenAlsoLift),
>	("NoDischarge", TokenNoDischarge),
>	("ElimRule", TokenElimRule),
>	("SimpleElimRule", TokenSimpleElimRule),
>	("Claim", TokenKeyword "Claim"),
>	("Goal", TokenKeyword "Goal"),
>	("Cut", TokenKeyword "Cut"),
>	("Else_T", TokenElse_T),
>	("Then_T", TokenThen_T),
>	("Normal", TokenNormal),
>	("Ctxt", TokenCtxt),
>	("JustParse", TokenJustParse),
>	("FiniteUniverse", TokenFiniteUniverse),
>	("InfiniteUniverse", TokenInfiniteUniverse),
>	("Cumulative", TokenCumulative),
>	("Closed", TokenClosed),
>	("AddToUniverse", TokenAddToUniverse),

>	-- language tokens.
>	("case", TokenCase),
>	("of", TokenOf),
>	("with", TokenWith),
>	("sig", TokenSig),
>	("str", TokenStr),

>	("El", TokenEl),
>	("kind", TokenKind),
>	("Type", TokenType)
>  ]



%-------------------------------------------------------------------------------

> 
> testTables 
>  = concat $ map mk [s1,s2]
>    where
>		s1 = ("<",">")
>		s2 = ("}","{")
>		mk se@(s,e) = [(s,TokenOpenBracket se),(e,TokenCloseBracket se)]

> t1 = basic_lex testTables

> test :: String -> IO ()
> test s
>  = putStr $
>    unlines $ 
>    map (\(s,t) -> show t ++ "      " ++ show s) $ 
>    takeWhile (\x -> case snd x of TokenEOF -> False ; _ -> True) $
>    tail $ 
>    iterate (\s -> basic_lex testTables (fst s)) (s,error "init")

