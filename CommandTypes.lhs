> module CommandTypes where

Command types...

> import SimpleTerm(SimpleTerm,SimpleType,CompRuleFunction)
> import SharedSyntax(IdOrNum, Name, Arity, BoundName)

> data TopCommand 
>  = JustParse TopCommand					-- for testing parser.
>										-- NB is recursive, be careful.
>  | Term 
>	SimpleTerm							-- just a term. 

>  | Normal 
>	SimpleTerm							-- normalise this.

>  | Conv 
>	ConvOpt [Def_Decl] SimpleTerm SimpleTerm	
>										-- conversion/unification in ctxt

>  | Claim					-- For introducing metavars.
>	ClaimForm (Maybe Name) SimpleTerm						

>  | Ctxt_op 					-- showing Context (or parts)
>	(Maybe IdOrNum)	

>  | Tactic 
>	TacticCommand

>  | Cut 
>	[Cut_Expression]

>  | Defs_Decls 
>	[Def_Decl]

>  | Local_Defs [Definition]				-- for proof-internal defs.

>  | ElimRule 
>	Name 								-- compile `Name' as an Elim oper
>	[CompRule]							-- with these comp rules.
>										-- (requires type inference?)
>										-- (or lego-style pre-decl.)

>  | SimpleElimRule 						-- SIMPLIFIED VERSION OF ABOVE (TMP)
>	Name			  					-- "inductive" type
>	Name  								-- "elimination" operator
>	Arity								-- arity of the elim op. 
>	[CompRuleFunction]					-- explicit fns, with types

>  | Inductive 
>	Declaration 						-- decl of name
>	[InductiveParameter] 				-- eg Theorems
>	[Declaration] 						-- parameters (obsolete? see below)
>	[Declaration]						-- constructors. (NB use FIELDS?)

>  | FunctionDef FunctionDefinition

>  | ModuleHeader String					-- marking the start of a module.

>  | CoercionDecl 
>		[Declaration]		-- (object) parameters
>		[Name]		 		-- names of prerequisite coercions (for lifting)
>		SimpleTerm			-- term to use as coercion
>		(Maybe SimpleTerm)	-- expected type, for information.

>  | InfiniteUniv 
>		[UnivOpt]
>		((String,SimpleTerm), SimpleTerm)	-- info for the lowest univ.
>		Name						-- (small) name - for names in T_{i+1}
>		Name						-- (big) name for the universes.

>  | FiniteUniv FiniteUniverse		-- defn below.
>  | AddToUniverse 
>		String			-- universe (base) name 
>		Name			-- name of inductive type to add.

>  | AddToPseudoUniverse		-- for user-defined universes.
>	SimpleTerm			-- the universe (type of codes)
>	SimpleTerm			-- the decode function. 
>	Name				-- name of type to produce code for

>  deriving Show


> data ClaimForm = PlainClaim | GoalClaim deriving Show

%-------------------------------------------------------------------------------

> data ConvOpt 
>  = StrictConv		-- do strict conversion test, no unification
>  | UnifyConv		-- do unifiability test
>  deriving (Show, Eq)

> data InductiveParameter
>  = Theorems		-- generate additional theorems NYI.
>  | Relation		-- inductive propositions, according to ZL rules
>  | Relation_LE	-- inductive propositions, with large elimination
>  | WantInd		-- generate easy theorems? NYI
>  | AlsoLift       -- also generate lifted version with default case
>  | NoDischarge	-- don't discharge the parameters; default is to do so.
>  deriving (Show, Eq)

%---------------------------------------

> data UnivOpt
>  = U_Cumulative	-- universes include everything from lower universes
>  | U_Closed		-- nothing more can be added.
>    deriving Show

> type UnivDecl = (Name, SimpleTerm)

> data FiniteUniverse
>  = FiniteUniverse
>	[UnivOpt]
>	Name 					-- name of this univ.
>	[Either UnivDecl FiniteUniverse]	-- name maps or recursive F_ Univs.
>    deriving Show

%---------------------------------------

> newtype Def_Decl
>  = Def_Decl (Either Definition Declaration)
>    deriving Show

> data Declaration
>  = Declaration [Name] SimpleTerm
>    deriving Show

> data Definition
>  = Definition [Name] [Def_PreBinding] SimpleTerm (Maybe SimpleTerm)

> instance Show Definition where
>	showsPrec _ (Definition n pbs t mty)
>	 = showString "Definition " . shows n . showString " <BS> " . 
>					shows t .  showString " " . shows mty

> type Def_PreBinding = ([BoundName], SimpleTerm)

> data Cut_Expression
>  = Cut_Expression Name SimpleTerm 
>    deriving Show

> data CompRule 
>  = CompRule SimpleTerm SimpleTerm SimpleType	-- lhs, rhs, kind.
>    deriving Show

> data TacticCommand
>  = Basic BasicCommand
>  | Then_T TacticCommand TacticCommand 
>  | Else_T TacticCommand TacticCommand
>  deriving Show

---
Basic commands 

The IdOrNum is a reference (to a metavar) in the context, absence of which
means the most recent metavar.

> data BasicCommand 
>  = Refine     (Maybe IdOrNum) SimpleTerm
>  | Intros     (Maybe IdOrNum) (Maybe SimpleTerm) [BoundName]
>  | Induction  (Maybe IdOrNum) IdOrNum
>  | Equiv      (Maybe IdOrNum) SimpleTerm
>  | Assumption	(Maybe IdOrNum)
>  | Immed      (Maybe IdOrNum)
>  deriving Show


%---------------------------------------
Function definitions 

are recursively structured, to allow nested local defs.

> data FunctionDefinition 
>  = FnDefinition [FunctionClause] LocalFunctionDefs	deriving Show 
> type LocalFunctionDefs = Maybe [FunctionDefinition]

> data FunctionClause 
>  = FnClause Name [SimpleTerm] SimpleTerm		deriving Show



%-------------------------------------------------------------------------------

TODO
more types for intros?

nb they could be SpTT specific.

eg intro on sigma.

