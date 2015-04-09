NB if get problems from big tuple in ghc, then use Array form of parser.

IMPORTANT: Parser Conflicts
The following are known, and expected.
	22 s/r
as of 7 Sept 1999, explained as


Shift/Reduce:
-------------

12 related to juxtaposition (a = a b | a)
6 related to similar problem with operators.
All resolved greedily - so ok.

+4 (7/9) for tacticals

Reduce/Reduce:
--------------

none!


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

> {
> module LF_Parser (
>		parseThenCall
>		, parseTermThenCall
>		, SimpleTerm
>		, SyntaxInfo(..)
>		) where

#if __GLASGOW_HASKELL__ < 302
#else
> import IOExts_(trace)
#endif

> import Data.Array
> import Control.Monad (ap)
> import Data.Char(isSpace)

> import BaseClasses(OkF(..), Embeddable(..), Fallible(..))
> import BaseTypes(isOk, fromFail, fromOk, elimOk)
> import BaseFunctions(fst3, snd3, thd3)

> import ParsingAux
> import CommandTypes
> import SimpleTerm
> import SharedSyntax
> import UserSyntax

> }

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Parser config

> %name lf_parser 
> %tokentype { Token }

> %monad { P } { thenP } { returnP }
> %lexer { lexer } { TokenEOF }

> %token 
>	-- tokenising error
>	-- bad_token       { TokenFail $$ }

>	-- numbers
>	NUM				{ TokenNum $$ }

> 		-- command words
>	DEFS_DECLS      { TokenDefsDecls }
>	BEGINF			{ TokenBeginFunction }
>	ENDF			{ TokenEndFunction }
>	"module"		{ TokenModule }
>	"Coercion"		{ TokenCoercion }
>	"Parameters"	{ TokenParameters }
>	"Prerequisites"	{ TokenPrerequisites }
>	"where"			{ TokenWhere }
>   	"Refine"		{ TokenRefine }
>   	"Intros"		{ TokenIntros }
>   	"Induction"		{ TokenInduction }
>   	"Assumption"	{ TokenAssumption }
>   	"Immed"			{ TokenImmed }
>   	"Equiv"			{ TokenEquiv }
>   	"Inductive"		{ TokenInductive }
>	"Constructors"	{ TokenConstructors }
>	WANT_IND		{ TokenWantInd }
>	THEOREMS		{ TokenTheorems }
>	"Relation"      { TokenRelation }
>	"Relation_LE"   { TokenRelation_LE }
>	"AlsoLift"      { TokenAlsoLift }
>	NO_DISCHARGE	{ TokenNoDischarge }
>	ELIM_RULE		{ TokenElimRule }
>	SIMPLE_ELIM		{ TokenSimpleElimRule }
>   	"Cut"			{ TokenKeyword "Cut" }
>   	"Claim"			{ TokenKeyword "Claim" }
>   	"Goal"			{ TokenKeyword "Goal" }
>   	ELSE_T			{ TokenElse_T }
>	THEN_T			{ TokenThen_T }
>	"Normal"		{ TokenNormal }
>	"Ctxt"			{ TokenCtxt }
>	JUST_PARSE		{ TokenJustParse }

>	"FiniteUniverse"	{ TokenFiniteUniverse }
>	"InfiniteUniverse"	{ TokenInfiniteUniverse }
>	"AddToUniverse"		{ TokenAddToUniverse }
>	"Cumulative"		{ TokenCumulative }
>	"Closed"			{ TokenClosed }


>	-- key words
>	"kind"          { TokenKind }
>	"Type"          { TokenType }
>	"El"            { TokenEl }
>	"case"			{ TokenCase }
>	"of"			{ TokenOf }
>	"with"			{ TokenWith }
>	"sig"			{ TokenSig }
>	"str"			{ TokenStr }

>	-- Misc Symbols
>	"::"            { TokenColonColon }
>	":!:"           { TokenColonExclColon }

>	-- punctuation
>	'_'             { TokenUnderSc }
>	':'             { TokenColon }
>	'='             { TokenEq }
>	','             { TokenComma }
>	'$'				{ TokenDollar }
>	"=="			{ TokenSymbol "==" }
>	'`'				{ TokenBacktick }

<>	'.'             { TokenDot }		DISABLE - special meaning removed
<>	'|'				{ TokenBar }

>	-- bracketing
>	'('             { TokenOP }
>	')'             { TokenCP }
>	'['             { TokenOB }
>	']'             { TokenCB }

>	'<'             { TokenOA }
>	'>'             { TokenCA }

>	'{'             { TokenOpenBracket  ("{","}") }
>	'}'             { TokenCloseBracket ("{","}") }
>	-- special treatment
>	-- which exact token depends on which notation is active.
>	-- need to think about the general implications.
>	-- eg remove { } from bracket table? 
>	-- < >  might need same treatment.



>	OPEN			{ TokenOpenBracket $$ }
>	CLOSE			{ TokenCloseBracket $$ }

>	-- identifiers
>	ID              { TokenId $$ }
>	BID             { TokenBindId $$ }
>	METAVAR_ID      { TokenMeta $$ }
>	UNIV_ID      	{ TokenUniv $$ }
>	OP				{ TokenSymbol $$ }


> %%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
The Productions

> start :: {TopCommand}
> start 
>   : top_command                   { $1 } 

<>   : top_command ';'               { $1 } 	
	-- unsure about need for this.
	-- maybe IS required if proper layout is implemented.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Top Commands are the ones which may include terms (hence need to be parsed)
but may not appear inside tactical expressions.

> top_command :: {TopCommand}
>   : JUST_PARSE top_command			{ JustParse $2 }
>   | tactical							{ Tactic $1 }
>   | DEFS_DECLS defs_decs 				{ Defs_Decls $2 } 
>   | ']' definitions					{ Local_Defs $2 }
>   | "Cut" cut_expressions           	{ Cut $2 }
>   | "Claim" claim_opt_id typing 	   	{ Claim PlainClaim $2 $3 }
>   | "Goal"  claim_opt_id typing 	   	{ Claim GoalClaim $2 $3 }
>   | "module" ID "where"              	{ ModuleHeader $2 }
>   | coercion_declaration				{ $1 }
>   | universe_declaration				{ $1 }
>   | declare_elim_rule					{ $1 }
>   | declare_inductive_type			{ $1 }
>	| function_by_clauses				{ $1 }

>   | ctxt_cmd							{ $1 }
>   | term_cmd							{ $1 }

%---------

> ctxt_cmd :: {TopCommand}
>  : "Ctxt" id_or_opt_num				{ Ctxt_op $2 }

> term_cmd :: {TopCommand}
>  : infixexp							{ Term $1 }
>  | "Normal" infixexp					{ Normal $2 }

>  | infixexp conv_cmd 					{ $2 [] $1 }
>  | '<' defs_decs '>' infixexp conv_cmd{ $5 $2 $4 }

> conv_cmd :: {[Def_Decl] -> SimpleTerm -> TopCommand}
>  : '='  infixexp						{ \dds t1 -> Conv StrictConv dds t1 $2 }
>  | "==" infixexp						{ \dds t1 -> Conv UnifyConv  dds t1 $2 }

%---------

> defs_decs :: {[Def_Decl]}
>   : defs_decs def_or_dec          { $1 ++ [$2] }
>   | def_or_dec					{ [$1] }			-- is this bad?

> def_or_dec :: {Def_Decl}
>   : declaration					{ Def_Decl (Right $1) }
>   | definition					{ Def_Decl (Left $1) }

> declarations :: {[Declaration]}
>   : declarations declaration		{ $1 ++ [$2] }
>   |                               { [] }

> declaration :: {Declaration}
>   : '[' decl_id_list typing ']'	{ Declaration $2 $3 }

> definitions :: {[Definition]}
>   : definitions definition		{ $1 ++ [$2] }
>   |                               { [] }

> definition :: {Definition}
>   : '[' decl_id_list typing '=' infixexp ']'
>									{ Definition $2 [] $5 (Just $3) }
>   | '[' decl_id_list pre_bindings '=' infixexp opt_typing ']'
>									{ Definition $2 $3 $5 $6 }

> pre_bindings :: { [Def_PreBinding] }
> pre_bindings 
>  : pre_bindings pre_binding       { $1 ++ [$2] }
>  |								{ [] }

> pre_binding :: { Def_PreBinding }
>  : '[' list_of_idOr_ typing ']'	{ ($2,$3) }

> decl_id_list :: {[Name]}
>   : decl_id_list ',' ctxt_name	{ $1 ++ [$3] }
>   | ctxt_name						{ [$1] }


%---------

> cut_expressions :: {[Cut_Expression]}
>   : cut_expressions cut_expression{ $1 ++ [$2] }
>   |								{ [] } 

NB not required to give explicit type in a cut expression, since we have
to check whether term is acceptable. But, we can still use casts.

> cut_expression :: {Cut_Expression}
>   : '[' ctxt_name '=' exp ']'		{ Cut_Expression $2 $4 }

%---------
Allow old form for a while (apr99).

> coercion_declaration :: {TopCommand}
>  : "Coercion" 
>		opt_coercion_parameters 
>		opt_coercion_prerequisites
>		'='
>		infixexp
>		opt_typing					{ CoercionDecl $2 $3 $5 $6 }

> opt_coercion_parameters :: {[Declaration]}
>  : "Parameters" declarations		{ $2 }
>  | 								{ [] }

> opt_coercion_prerequisites :: {[Name]}
>  : "Prerequisites" decl_id_list	{ $2 }
>  |                             	{ [] }

%---------

> declare_inductive_type :: {TopCommand}
>   : "Inductive" 
>		declarations 		-- parameters - NB these are unmarked, unlike lego
>		declaration 		-- name
>		inductive_parameters 
>	  "Constructors"
>		declarations		-- constructors
>									{ Inductive $3 $4 $2 $6 }

> inductive_parameters :: {[InductiveParameter]}
>   : inductive_parameters inductive_parameter 
>									{ $1 ++ [$2] }
>   |								{ [] }

> inductive_parameter :: {InductiveParameter}
>   : WANT_IND						{ WantInd }
>   | THEOREMS						{ Theorems }
>   | "Relation"                    { Relation }
>   | "Relation_LE"                 { Relation_LE }
>   | "AlsoLift"                    { AlsoLift }
>	| NO_DISCHARGE					{ NoDischarge }

%-------------------

> declare_elim_rule :: {TopCommand}
>   : ELIM_RULE ctxt_name comp_rules
>                                   { ElimRule $2 $3 } 
>   | SIMPLE_ELIM ctxt_name ctxt_name NUM simple_c_rules	
>                                   { SimpleElimRule $2 $3 $4 $5 } 

> comp_rules :: {[CompRule]}
>   : comp_rules '[' infixexp '=' infixexp typing ']'
>									{ $1 ++ [CompRule $3 $5 $6] }
>   |								{ [] }

> simple_c_rules :: {[ CompRuleFunction ]}
>   : simple_c_rules '[' ctxt_name NUM '=' infixexp typing ']'
>									{ $1 ++ [ CRF $3 $4 $6 $7 ] }
>   |								{ [] }


%-------------------
This is inherently recursive. 

> function_by_clauses :: {TopCommand}
>  : fn_def							{ FunctionDef $1 }

> fn_def :: {FunctionDefinition}
>  : BEGINF clauses opt_where ENDF 	{ FnDefinition $2 $3 }

Must have at least one clause.
But I allow "where" to have no defs. Incongruous? (Maybe!)

> clauses :: { [ FunctionClause ] }
>  : clause clauses_				{ $1 : $2 }

> clauses_ :: { [ FunctionClause ] }
>  : ',' clause clauses_			{ $2 : $3 } 
>  |								{ [] }

> opt_where :: {LocalFunctionDefs}
>  : "where" more_defs				{ Just $2 }
>  | 								{ Nothing }

> more_defs :: {[FunctionDefinition]}
>  : more_defs fn_def				{ $1 ++ [$2] }		-- why this order?
>  | 								{ [] }


> clause :: { FunctionClause }
>  : ctxt_name term_list '=' infixexp		{ FnClause $1 $2 $4 } 


Q: this provides easier route than application? 

NB I don't distinguish constructors here. Maybe I should?
  - but this requires context. (could hide this inside a predicate, in Monad)

  - Use aexp, so we don't get lists of terms formed in to Applications.
  - EXPERIMENTAL. 
  - aexp - +9 sr. 

> term_list :: {[SimpleTerm]}
>  : list_term term_list 			{ $1 : $2 }
>  | list_term						{ [$1] }

> list_term :: {SimpleTerm}		-- as appearing in simplest term/constr list.
>  : exp                            { $1 } 



%-------------------------------------------------------------------------------
Tactical Commands - as for lego.
Not really implemented yet.

> tactical :: {TacticCommand} 
>   : basic_cmd                     { Basic $1 }
>   | tactical THEN_T tactical		{ Then_T $1 $3 }
>   | tactical ELSE_T tactical		{ Else_T $1 $3 }
>   | '(' tactical ')'				{ $2 } 

%-------------------------------------------------------------------------------
Basic Commands

These are the ones which occur inside tactical expressions.

> basic_cmd :: {BasicCommand}
>   : id_or_opt_num "Refine"    infixexp	{ Refine     $1 $3 }
>   | id_or_opt_num "Intros"    intros_args	{ Intros     $1 Nothing $3 }
>   | id_or_opt_num "Intros"    '{' exp '}' intros_args	
>                                           { Intros     $1 (Just $4) $6 }
>   | id_or_opt_num "Induction" id_or_num	{ Induction  $1 $3 }
>   | id_or_opt_num "Equiv"     infixexp	{ Equiv      $1 $3 } 
>   | id_or_opt_num "Immed"					{ Immed      $1 }
>   | id_or_opt_num "Assumption" 			{ Assumption $1 }

%---------

> id_or_opt_num :: { Maybe IdOrNum }
>  : id_or_num 						{ Just $1 }
>  | 								{ Nothing }

> id_or_num :: { IdOrNum }
>  : ctxt_name						{ Left $1 }
>  | NUM							{ Right $1 }

> claim_opt_id :: {Maybe Name}
>  : ctxt_name						{ Just $1 }
>  | 								{ Nothing }

%---------

> intros_args :: {[BoundName]}
>  : intros_args idOr_				{ $1 ++ [$2] }
>  | 								{ [] }

%-------------------------------------------------------------------------------
UNIVERSES

> universe_declaration :: {TopCommand}
>  : finite_universe					{ FiniteUniv $1 }
>  | infinite_universe					{ $1 }
>  | decl_in_universe 					{ $1 }

> decl_in_universe :: {TopCommand}
>  : "AddToUniverse" UNIV_ID ctxt_name	{ AddToUniverse $2 $3 }
>  | "AddToUniverse" ':' exp '>' exp '>' ctxt_name	
>										{ AddToPseudoUniverse $3 $5 $7 }

<> univ_id_list :: {[String]}
<>  : univ_id_list ',' UNIV_ID			{ $1 ++ [$3] }
<>  | UNIV_ID							{ [$1] }

> universe_opts :: {[UnivOpt]}
>  : universe_opts "Cumulative"			{ $1 ++ [U_Cumulative] }
>  | universe_opts "Closed"				{ $1 ++ [U_Closed] }
>  |									{ [] }



%---------------------------------------
Infinite 
	- eg Type(i)
	- univ_base is the "base case" for recursion in universe defn.
	- optional as to whether inclusion happens.
	- QQ: if not include by rule, can we have coercions between them? 

> infinite_universe :: {TopCommand}
>  : universe_opts 
>		"InfiniteUniverse" 
>		univ_base
>		univ_or_id univ_or_id			{ InfiniteUniv $1 $3 $4 $5 }

> univ_base :: {((String,SimpleTerm), SimpleTerm)}
>  : '{' ID '=' infixexp ',' infixexp '}'	{ (($2, $4), $6) }

> univ_or_id :: {Name}
>  : UNIV_ID	{ Plain $1 }
>  | ID			{ Plain $1 }



%---------------------------------------
Finite
	- declare whole lot - from the top down.
	- means we can have multiple hierarchies...

> finite_universe :: {FiniteUniverse}
>  : universe_opts
>		"FiniteUniverse"
>		univ_or_id 
>		'{' universe_contents '}'			{ FiniteUniverse $1 $3 $5 }

> universe_contents :: {[Either UnivDecl FiniteUniverse]}
>  : universe_contents ',' finite_universe	{ $1 ++ [Right $3] }
>  | universe_contents ',' univ_decl 		{ $1 ++ [Left $3] }
>  |										{ [] }

> univ_decl :: {UnivDecl}
>  : '[' ID '=' infixexp ']'				{ (Plain $2, $4) }

might want to extend this to allow operator decls.

since nested, from outermost, then doesn't make sense to have infinite...

though, could say that outer id is start of an infinite sequence.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Context names

Forms of name which can be used as Ctxt labels. 
Backtick form omitted - not really relevant? 
It also enforces use of sectioned form for names of operators.

> ctxt_name :: {Name} 
>  : ID 			{ Plain $1 }
>  | '(' OP ')'		{ Operator $2 }




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
TERMS.

nomenclature borrowed from Simon Marlow's GHC grammar.

> exp :: {SimpleTerm}
>  : infixexp						{ $1 }
>  | infixexp cast infixexp			{ $2 $1 $3 }

---
Casting operators.

> cast :: {SimpleTerm -> SimpleTerm -> SimpleTerm}
>  : "::"							{ \a b -> Ext_S (SoftCast a b) }
>  | ":!:"							{ \a b -> Ext_S (HardCast a b) }


OR CASTS

%-------------------

> infixexp :: {SimpleTerm}
>  : exp10 							{ $1 }
>  | infixexp qop exp10				{ $2 $1 $3 }


%-------------------
An Operator

Can be simple, or an operator prefixed with series of (ID op) chains - 
eg for dotted module name.

> qop :: {SimpleTerm -> SimpleTerm -> SimpleTerm}
>  : OP						{ \l -> Op_S l (mk_op $1) } 
>  | '`' ID '`'				{ \l -> Op_S l (Name_S (Backticks $2)) } 
>  | '`' ID OP qop_aux '`'	{ \l -> Op_S l (Op_S (mk_plain $2) (mk_op $3) $4) } 

> qop_aux :: {SimpleTerm}
>  : OP						{ mk_op $1 }
>  | ID						{ mk_plain $1 }
>  | ID OP qop_aux		 	{ Op_S (mk_plain $1) (mk_op $2) $3 }

This form is workable, resulting in not many more SR, but probably confusing.
The unbracketed form DOES give a lot of SR ambiguities.
Currently disabled.

<>  | '(' exp10 plain_qop qop ')'		{ error "qop exp10" }



%---------------------------------------
Exp10

extensions to basic terms, eg let, case, if, do - all go here.

> exp10 :: {SimpleTerm}
>  : fexp								{ $1 } 
>  | '[' ctxt_name '=' exp ']' infixexp	{ Let_S $2 $4 $6 }
>  | case_expression					{ $1 }

>  | manifest_rec						{ $1 }
>  | exp10 "with" ctxt_name '=' aexp	{ Ext_S (With $1 $3 $5 Nothing) }


Here, choice on what the let-body should be. exp10 gives low SR, exp highest.
Infixexp gives reasonable medium. 

NOTE: In TT, might want binding of Let to be over a signature. Using infixexp
means we'd have to bracket the signature. Try that for now. 

%---------------------------------------
To be moved down later

> manifest_rec :: {SimpleTerm}
>  : "str" '<' exp defn_fields '>' 
>								{ mk_rec $3 (reverse $4) }
>  | "sig" '<' exp sig_fields '>'
>								{ mk_sig $3 (reverse $4) }

> decl_field :: {(Name,SimpleTerm)}
>  : ',' ctxt_name ':' exp			{ ($2,$4) }

> defn_field :: {(Name,SimpleTerm,SimpleTerm)}
>  : ',' ctxt_name '=' exp ':' exp	{ ($2,$4,$6) }

> sig_field :: {(Name, OpaqueOrManifest SimpleTerm)}
>  : decl_field					{ (fst $1, Opaque (snd $1)) }
>  | defn_field					{ (fst3 $1, Manifest (snd3 $1) (thd3 $1)) }

> defn_fields :: {[(Name,SimpleTerm,SimpleTerm)]}
>  : defn_field defn_fields 		{ $1 : $2 }
>  | defn_field						{ [$1] }
> sig_fields :: {[(Name,OpaqueOrManifest SimpleTerm)]}
>  : sig_field sig_fields 			{ $1 : $2 }
>  | sig_field						{ [$1] }


TO_DO - decide how to phrase this. 
problem - using dislike strs for same info (manifest vs Rec, both have type)

%---------------------------------------

> fexp :: {SimpleTerm}
>  : fexp aexp 						{ Ap_S $1 $2 }
>  | aexp							{ $1 }
>       -- The order of these rules makes a BIG difference 
>       -- (fexp ---> aexp) *MUST* come last! 


> aexp  :: {SimpleTerm}
>   -- terminal constructions
>   : "Type"                        { mk_plain "Type" }
>   | "kind"                        {% failP "kind" } -- if A : kind notation...

RETHINK THIS ? 

>   | "El" parenthesis_initial_expr	{ mk_El $2 }
>   | "El" ID						{ mk_El (mk_plain $2) }
>   | "El" UNIV_ID					{ mk_El (mk_plain $2) }
>   | "El" METAVAR_ID				{ mk_El (mk_plain $2) }

NOTE:
	Bracketing of El argument is required in order to limit ambiguities.
	(Remember: in prettyprinting, El must also be bracketed)
	Often, there is no confusion about El terms, so we can omit the El.
	  - so having to add explicit brackets isn't a problem.
	But could be an annoyance if El is enforced...

	* Special case for El on ID added, plus UNIV_ID and METAVAR_ID

>   | ID                            { mk_plain $1 }
>   | METAVAR_ID                    { mk_plain $1 }
>   | UNIV_ID                   	{ mk_plain $1 }

>   -- application
>       -- primarily handled by fexp

>   -- binding
>		-- `binding' handles the first bit.
>   | binding infixexp               { $1 $2 }



>   -- anything that begins with a '(', including DP bindings.
>   | parenthesis_initial_expr		{ $1 }



%---------------------------------------
Expressions Starting with Parenthesis.

 - things that can happen after "( infixexp"
 - the more_X productions ensure that the epsilon of parenthesis_end is used.
   (otherwise, it gets hidden by the previous epsilon of dollar_terms)
   (the distinction is mainly cosmetic though...)

 - Extensive use of higher-order functions, to facilitate passing of previous
   subtree results. 

 - Could need to cause a fail if terms contain a "_" - but since these are
   not valid variables, it isn't a great problem - name validity will catch
   it. Besides, "_" is useful in pattern matching applications.

 - Main problem - only allow ID or "_" as binder var.

 - Haven't worked out how to do this checking inside the P monad, hence the 
   hacky "is_dp_fail" at the top level.

> parenthesis_initial_expr :: {SimpleTerm}
>  : '(' termOr_ parenthesis_end		
>									{% elimOk failP returnP (is_dp_fail ($3 $2))
>									}
>  | '(' OP ')'						{ Name_S (Operator $2) } 

WHAT HERE? op chg to normal name?  COULD USE ctxt_name PRODUCTION.

> termOr_ :: {SimpleTerm}
>  : exp							{ $1 }
>  | '_'							{ underscore_name }
>			-- combined term and "_", for parsing commaterms.

> parenthesis_end :: {SimpleTerm -> SimpleTerm}
>  : dollar_terms         ')'		{ \t -> mk_ra_ap t $1 }
>  | commaterms_maybe_dp			{ $1 }
>		-- one term in ( ) is handled by commaterms_maybe_dp

> dollar_terms :: {[SimpleTerm]}
>  : '$' infixexp more_dollar_terms	{ $2 : $3 }
> more_dollar_terms :: {[SimpleTerm]}
>  : '$' infixexp more_dollar_terms	{ $2 : $3 }
>  | 								{ [] }
>			-- need at least one dollar
>			-- this is to avoid confusion with last rule of parenthesis_end 

> commaterms_maybe_dp :: {SimpleTerm -> SimpleTerm}
>  : commaterms dp_ending		{ \t -> $2 (t : $1) }
>  | commaterms ')'				{ \t -> case $1 of
>											[] -> Pars_S t
>											ts -> foldr1 (ap2 ",") (t:ts) }
>		-- a comma-sequence ends with ")" (tuple) or ":" (binding)
>		-- the latter includes the basic (Expr) case, which is marked by Pars_S.

> commaterms :: {[SimpleTerm]}
>   : ',' termOr_ commaterms		{ $2 : $3 }
>   |                               { [] }


> dp_ending :: {[SimpleTerm] -> SimpleTerm}
>  : typing ')' opt_dp_arrow infixexp	
>									{ \ts -> check_all_ids_and_bind $3 ts $1 $4}
>		-- generic collection of type and body, irrespective of binder.

> opt_dp_arrow :: { ([BoundName],SimpleTerm) -> SimpleTerm -> SimpleTerm}
>  : OP								{% check_dependent_arrow ("(",")") $1 }
>  | 								{ mkBinding By_DP }







----------------------------------------

> list_of_idOr_ :: {[BoundName]}
>   : list_of_idOr_ ',' idOr_	 	{ $1 ++ [$3] }
>   | idOr_							{ [$1] }
> 
> idOr_ :: {BoundName}
>   : ID                            { Bound (Plain $1) }
>   | '_'                           { Underscore }

> bind_var :: {([BoundName],SimpleTerm)}
>   : list_of_idOr_ typing      	{ ($1,$2) }

> typing :: {SimpleTerm} 
>  : ':' infixexp					{ $2 }
>  | ':' "El" '$' infixexp			{ mk_El $4 }

> opt_typing :: {Maybe SimpleTerm}
>  : typing						{ Just $1 } 
>  |                               { Nothing }

> binding :: {SimpleTerm -> SimpleTerm}
>  : BID bind_var ']'               { mkBinding (By_Constr $1) $2 }
>  | bracket_binding opt_arrow		{ fst $1 }

> opt_arrow :: {Maybe Name}
>  :								{ Nothing }
>  -- | OP							{ Just (Operator $1) }


> bracket_binding :: {(SimpleTerm -> SimpleTerm, BindVariety)}
> bracket_binding
>  : '[' bind_var ']'               { mk_binding By_FO $2 }
>  | '{' bind_var '}'               { mk_binding (By_Brackets "{" "}") $2 }
>  | '<' bind_var '>'               { mk_binding (By_Brackets "<" ">") $2 }

>  -- extended binding forms
>  | OPEN bind_var CLOSE			{% mk_brackets $1 $3 $2 }

>  -- SEE parenthesis handling stuff.


NOTE: multiple ids in "(a:A)..."
 * can't do directly since it looks like a tuple term.
 * NB this id sequence allows "_", which isn't allowed in tuples. 
 * could do some low-level checking using "{%" syntax
 * but, rewrite involves modification of clean binding abstr.


<>	-- last chance (TESTING)
<>		-- allow (A)B juxtaposition when A is a aexp
<>		-- this might be tricky to explain?
<>	-- NB it interferes with normal application stuff.
<>	| '(' aexp ')'				{ mk_dp Nothing $2 }

MORE TO COME... 

NB that all bindings are of form <symbols> id : .... <symbols>
So - patterns of opening symbols won't be confused, eg <<x:A>>B...
There might be some problem with (( - but we will see...



%---------------------------------------
Case.

The bracketing of arms helps to reduce SR problems. 
It looks like non-OP symbols are needed, to avoid more SR confusion. 
'=' is probably a good choice - since it can't occur elsewhere in an expr.

> case_expression :: {SimpleTerm}
> : "case" term_list "of" '{' case_arms	'}' 
>									{ Ext_S (Case $2 $5) }

> case_arms :: { [ ([SimpleTerm], SimpleTerm) ] }
>  : case_arm ',' case_arms			{ $1 : $3 } 
>  | case_arm						{ [$1] }

> case_arm :: { ([SimpleTerm], SimpleTerm) }
>  : term_list '=' exp				{ ($1, $3) }





> {

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
AUXILIARY CODE FOR THE GRAMMAR RULES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Main external calls for parser
 - general combination 

> type Parse = P TopCommand
> lf_parser :: Parse


> parseThenCall 
>  :: (Fallible m, Monad m) => 
>        (TopCommand -> m a) 
>     -> (SimpleTerm -> m a) 
>     -> BracketTable
>     -> String 
>     -> m a
> parseThenCall cf tf ts s
>  = do
>		let i_tables = (compileBrackets $ map fst ts, 
>		                compileArrows   ts)
>		c <- embed (run_parser lf_parser i_tables s)
>		case c of
>			Term t -> tf t
>			cmd    -> cf cmd
>    where
>		compileArrows ts = [ (a,se) | (se, Just a) <- ts ]


---
slightly more useful - branches on Term or not.

> parseTermThenCall 
>  :: (Fallible m, Monad m) => 
>				(SimpleTerm -> m a) -> BracketTable -> String -> m a
> parseTermThenCall 
>  = parseThenCall (\cmd -> fail_with_msg $ "Got cmd: " ++ show cmd)


%-------------------------------------------------------------------------------
The `glue' for the parser: a monad.

left un-protected and not an instance of Monad so that we could use this 
with older haskell...

> type SyntaxInfo = ( BracketTable, ArrowInfo )
> type InternalSyntaxInfo = ( BracketInfo, ArrowInfo )

> type P a = String -> InternalSyntaxInfo -> Int -> OkF a

> thenP :: P a -> (a -> P b) -> P b
> m `thenP` k = \s ts l -> m s ts l >>= \a -> k a s ts l

> returnP :: a -> P a
> returnP a = \s ts l -> Ok a

> failP :: String -> P a
> failP m = \s ts l -> Fail m

> syntaxInfo :: P InternalSyntaxInfo
> syntaxInfo s i l = Ok i

---
`lexer' lifts the low level lexer into this monad.

> lexer :: (Token -> P a) -> P a
> lexer cont s tables
>  = let (rest,token) = basic_lex (fst tables) s in
>    case token of
>      TokenNL  -> \line -> lexer cont rest tables (line+1)
>      t        -> cont t rest tables

---
`run_parser' does this, with exception for strings that begin with '[' -
they could be definition/declaration mixes. This is to avoid complicating the
parser.

> run_parser :: P a -> InternalSyntaxInfo -> String -> OkF a
> run_parser parser tables s
>  | take 1 (dropWhile isSpace s) == "["
>     = case (run_parser_ s) of
>			ok@(Ok _) -> ok
>			Fail m    -> change_msg (\_ -> m) $ 
>						 run_parser_ (prefix_as_defs_decls s)
>  | otherwise
>     = run_parser_ s
>  where
>	run_parser_ s = parser s tables 1
>	prefix_as_defs_decls s = defs_decls_string ++ " " ++ s


---
Required by the parsing template:

> happyError :: P a		-- any P.
> happyError = \s _ i -> Fail (
>    "Parse error in line " ++ show (i::Int) ++ ": \n|" ++ take 100 s ++ "|\n")

> happyErrorToken :: Token -> P a
> happyErrorToken tk = \s _ i -> Fail (
>    "Parse error in line " ++ show i ++ ", at token " ++ show tk 
>	   ++ ":\n|" ++ take 100 s ++ "|\n")


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

---

> check :: String -> String -> a -> String -> P a
> check inp exp r e 
>  | inp == exp = returnP r
>  | otherwise  = failP $ "Expected " ++ exp ++ " in " ++ e

---
Fixed symbol mappings

Could make these FULLY configurable? 

> case_rhs_separator = "->" :: String

> case_clause_separator = "|" :: String 

> fn_clause_separator = "|" :: String 


---
mk_El - representation of El.

> mk_El = Ap_S (mk_plain "El") 

> mk_op = Name_S . Operator

> mk_plain = Name_S . Plain


---
glue together names. (no structure)

> mk_dot q e = q ++ "." ++ e 

---

> mk_binding :: BindVariety -> ([BoundName],SimpleTerm)
>                               -> (SimpleTerm -> SimpleTerm, BindVariety)
> mk_binding k (vs,ty) = (mkBinding k (vs,ty), k)


---
ap2 - build application of f to x & y

> ap2 f x y = Ap_S (Ap_S (mk_plain f) x) y

---
`mk_ra_ap'
  - construct the right-assoc application of head term to list of terms.

> mk_ra_ap f []      = f
> mk_ra_ap f (r1:rs) = Ap_S f (mk_ra_ap r1 rs)


---

> mk_rec l []     = l
> mk_rec l (f:fs) = Ext_S (Rec (mk_rec l fs) (fst3 f) (snd3 f) (thd3 f))
> mk_sig l []     = l
> mk_sig l (f:fs) = Ext_S (Sig (mk_sig l fs) (fst f) (snd f))

---
`check_all_ids_and_bind'
  - would like to return a result (OkF SimpleTerm) or (P SimpleTerm)
  - but getting this to work with other Monad stuff in parser is not easy.
  - hence, codify an error via mk_fail_dp, detected higher-up with is_dp_fail

> fail_dp_creation_string = "Failed DP"
> mk_fail_dp m = Name_S $ Plain $ fail_dp_creation_string ++ m
> is_dp_fail :: SimpleTerm -> OkF SimpleTerm
> is_dp_fail t@(Name_S (Plain n))
>  | prefix == fail_dp_creation_string = Fail suffix
>  | otherwise                         = Ok t
>  where
>		(prefix, suffix) = splitAt (length fail_dp_creation_string) n
> is_dp_fail t = Ok t

---

> mk_brackets 
>  :: BracketPair -> BracketPair -> ([BoundName],SimpleTerm) 
>				-> P (SimpleTerm -> SimpleTerm, BindVariety)
> mk_brackets open close r
>  | open == close = returnP $ mk_binding (By_Brackets (fst open) (snd open)) r 
>  | otherwise = failP $ "Mismatch in bracket symbols: " ++ show (open,close)

---



> check_all_ids_and_bind 
>  :: (([BoundName],SimpleTerm) -> SimpleTerm -> SimpleTerm) 
>		-> [SimpleTerm] -> SimpleTerm -> SimpleTerm -> SimpleTerm

> check_all_ids_and_bind bf ns ty t
>  | null bad_ids = bf (map fromOk ok_ids, ty) t
>  | otherwise    = mk_fail_dp $ fromFail $ head bad_ids
>  where
>		is_ok_id (Name_S (Plain "_")) = Ok $ Underscore
>		is_ok_id (Name_S n)           = Ok $ Bound n
>		is_ok_id t                    = Fail $ "bad id term: " ++ show t
>		(ok_ids, bad_ids) = span isOk $ map is_ok_id ns

DODGY - have to look at this mechanism again (for "_") 


---

> type TEMP_Operator = String

> check_dependent_arrow :: BracketPair -> TEMP_Operator 
>					-> P (([BoundName],SimpleTerm) -> SimpleTerm -> SimpleTerm)
> check_dependent_arrow se op 
>  = trace ("CDA " ++ show se ++ "," ++ show op ++ "\n") $ 
>    syntaxInfo `thenP` \i -> 
>    let pair = lookup op (snd i) in
>    if pair == Just se
>      then returnP (trace "OK\n" $ mkBinding $ uncurry By_Brackets se)
>      else failP (trace "NOT OK\n" $ "Optional arrow error: "++op++" is used with "++show pair)

NOTE: might want to generalise.
this stuff is about handling of "(" ")" as bracketing symbols.

Q: all brackets have std arrow form for non-dependent? 

> }

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
NOTES:
