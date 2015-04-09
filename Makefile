################################################################################
# Contents list
#
# Build-specific customisation
# Site-specific customisation
# GHC Compiler settings & configuration
# GHC flag groups and command-to-run
# Make rules
# etc

################################################################################
# Build-specific customisation

# User preferences are set by local file Build.Makefile 
# Defaults are given below, and will be overridden if set in Build.Makefile

# Explanation:
#   OPTIONS = build options, including CPP symbols
#   PARSER  = select form of parser to use, --ghc or --array
#   ghc_v   = which version of GHC, eg 4_03

# defaults.
OPTIONS = -DPRODUCTION -D__HASKELL98__
ghc_v   = 7_linux
PARSER  = --ghc

# this setting is up to individuals, so allow it to be set easily, per build
EDIT=vi

# now, add the user's build settings
include Build.Makefile



################################################################################
# Site-specific customisation
#
# Durham-specific settings for Solaris etc are defaults
# These are overridden by setting them in Local.Makefile 
# Other sites will need to do this. 
#
# Site settings should include location of ghc executables, in vars ghc_vXYZ.
#   Eg location of ghc 4.05 is in variable ghc_v4_05

MAKE=/usr/local/gnu/bin/make			# for recursion.
TIME=/usr/5bin/time
PERL=perl5

## TODO: what happens with test? 
# some times, gnu test is the version to use
TEST=/usr/bin/test

# then override by any specific site settings
include Local.Makefile




################################################################################
# GHC Compiler options, per version
#
# NB the V_GHC symbols are for C code...


# TODO - settings on a per-compiler basis.

flags_2_10 = -syslib ghc -syslib hbc -DV_GHC=210 -optc-DV_GHC=210 \
		-fshow-import-specs 

flags_3_02 = -syslib posix -syslib exts -DV_GHC=302 -optc-DV_GHC=302

flags_4_03 = -syslib posix -syslib exts -DV_GHC=403 -optc-DV_GHC=403 

# ghc_4_03_DEBUG = -syslib posix -syslib exts -DV_GHC=403 -optc-DV_GHC=403

flags_4_04 = -syslib posix -syslib exts -DV_GHC=404 -optc-DV_GHC=404 

flags_4_05 = -syslib posix -syslib exts -DV_GHC=405 -optc-DV_GHC=405

flags_4_06 = \
		-syslib posix -syslib lang -syslib data -syslib concurrent\
		-DV_GHC=406 -optc-DV_GHC=406 
				  # -freport-disallowed-unfoldings = unrecognised!


flags_4_06_linux = \
		-syslib posix -syslib lang -syslib data -syslib concurrent\
		-L/usr/lib \
		-DV_GHC=405 -optc-DV_GHC=405 

flags_4_08 = \
		-syslib posix -syslib lang -syslib data -syslib concurrent\
		-DV_GHC=408 -optc-DV_GHC=408 

flags_5_02 = \
		-package posix -package lang -package data -package concurrent\
		-DV_GHC=502 -optc-DV_GHC=502 

flags_5_04 = \
		-package posix -package lang -package data \
		-package text -package concurrent

flags_5_04_linux = ${flags_5_04}

flags_6_linux = -cpp -package ghc-6.12.1 -package base -package haskell98 -I/usr/lib/ghc-6.12.1/include/rts

flags_7_linux = -cpp -package ghc-7.6.3 -package base -hide-package haskell98


################################################################################
# ghc options

DUMP = -dshow-passes # -ddump-spec 
#DUMP = -dshow-passes -fshow-specialisations -fshow-import-specs
#DUMP = -dshow-passes -ddump-stg -ddump-deriv -ddump-absC -dppr-user

# degrees of warning from ghc.

GHC_BASIC_FLAGS = -fwarn-incomplete-patterns -fwarn-unused-binds

GHC_WORRY_FLAGS = ${GHC_BASIC_FLAGS}\
				  -fwarn-duplicate-exports -fwarn-unused-matches\
				  -fwarn-unused-imports -fwarn-overlapping-patterns\
				  -fwarn-name-shadowing

GHC_VERY_WORRIED_FLAGS = -Wall

# and the active choice.
GHC_FLAGS = ${GHC_BASIC_FLAGS}



################################################################################
# Constructing the command
#

# is this still needed?
ghc_vHUGS = echo Use Hugs
ghc_v4_05 = ghc
ghc_v6_linux = ghc
ghc_v7_linux = ghc
HAPPY = happy

fixed_opts = -cpp -H30M 
which_ghc  = ${ghc_v${ghc_v}} ${flags_${ghc_v}} ${fixed_opts}

compiler_cmd = ${which_ghc} ${DUMP} ${GHC_FLAGS} ${OPTIONS} ${EXTRA} 
ghc = ${TIME} ${compiler_cmd} 



################################################################################
# make rules

.SUFFIXES: .hi .o .hs .lhs .ly .c .hc

.hs.hi :
	${ghc} -c $*.hs
.lhs.hi :
	${ghc} -c $*.lhs

.hs.o :
	${ghc} -c $*.hs
.lhs.o :
	${ghc} -c ${HC_OPTS_$*} $*.lhs
.hc.o :
	${ghc} -c $*.hc

.o.hi :
	@ # .o -> .hi for $* 

.c.o :
	${ghc} -c $*.c


################################################################################
# Main deps

# extra stuff appended to exec name, to indicate which kind of exec.
TAG=${shell echo ${OPTIONS} | perl -pe 's/\s+-/,/g;s/^-//;s/^(.)/\1/;'}


# doesn't like this separately! lf  : lf@${TAG} 
# nb the *.o _is_ required, because the .o recomp might not trigger trans
# recomp in higher, but it still means linkage is needed.

lf lf@${TAG} : Build.Makefile Main.o ghc-hooks.o *.o 
	@ echo OPTIONS SETTINGS ARE
	@ cat Build.Makefile
	@ echo
	@ echo COMPILER IS ${ghc_v}
	@ echo
	${PERL} produce-identifiers "${TAG}" "${compiler_cmd}"
	- rm -f identifiers.o
	gcc -c identifiers.c
	${ghc} -o lf@${TAG} *.o 
	strip lf@${TAG}
	if ${TEST} lf@${TAG} != lf ; then rm -f ./lf ; ln -s lf@${TAG} lf ; fi



################################################################################
# compiling flavours - intended to call once (just use make after)

very_opt :
	echo setting options for Very Optimised
	echo "-O -fvia-C -O2-for-C -DPRODUCTION" >> Build.Makefile
	${MAKE} all
	strip lf@${TAG}

opt :
	echo setting options for basic optimised
	echo "-O -DPRODUCTION" >> Build.Makefile
	${MAKE} all 
	strip lf@${TAG}

scc-prof :
	echo setting options for plain prof, using scc\'s only.
	echo "-prof -DPRODUCTION" >> Build.Makefile
	${MAKE} all

scc-opt-prof :
	echo setting options for plain prof with optimisations, using scc\'s only.
	echo "-prof -O -DPRODUCTION" >> Build.Makefile
	${MAKE} all

prof :
	echo setting options for auto-scc profiling
	echo "-prof -auto -DPRODUCTION" >> Build.Makefile
	${MAKE} all

all-prof :
	echo setting options for auto-all profiling
	echo "-prof -auto-all -DPRODUCTION" >> Build.Makefile
	${MAKE} all

auto-opt :
	echo setting options for auto-scc profiling with optimisations
	echo "-prof -auto -O -DPRODUCTION" >> Build.Makefile
	${MAKE} all
#	rm Main.o 
#	${MAKE} PO='-prof -O' Main.o	# special case for Main. IOBase_con_info ?
#	${MAKE} PO='-prof -auto -O'	# and link.


################################################################################
# exceptions to make rules

parser : LF_Parser.hs			# the one needed by Hugs.

## NOTE about ghc & Happy
## --array XOR --ghc


REPEAT_CAFS =${shell \
	if test ${PARSER} = "--array" ; then \
	echo ${PERL} ${PLASTIC_LIB}/../bin/mk_repeat_cafs LF_Parser.${PARSER}.hs ;\
	else echo echo Not doing mk_repeat_cafs ; fi; }

REMOVE_IMPORTS=${shell \
	if test ${ghc_v} = 2_10 ; then \
	echo '${PERL} -i -ne "print unless /^import Prel/;" LF_Parser.${PARSER}.hs' ;\
	else echo echo ; fi; }

# Now need Happy 1.6
LF_Parser.${PARSER}.hs : LF_Parser.ly
	# making array parser for ghc
	${HAPPY} \
		--outfile=LF_Parser.${PARSER}.hs --info=LF_Parser.info \
		${PARSER} LF_Parser.ly 
	${REPEAT_CAFS} 
	${REMOVE_IMPORTS} 
	${PERL} -i -ne "print unless /simonmar/;" LF_Parser.${PARSER}.hs


LF_Parser.hs : LF_Parser.ly
	# making list parser for Hugs.
	rm -f foo.ly
	grep -v "import Prel" LF_Parser.ly > foo.ly
	${HAPPY} --outfile=LF_Parser.hs --info=LF_Parser.info foo.ly 
	rm -f foo.ly

PARSER_DEPS = ParsingAux.hi CommandTypes.hi SimpleTerm.hi \
				SharedSyntax.hi UserSyntax.hi

PARSER_STACK = -K40M 
PARSER_STACK = 
LF_Parser.o : LF_Parser.${PARSER}.hs ${PARSER_DEPS}
	# compiling LF_Parser.${PARSER}.hs WITHOUT optimisation, and heap big heap.
	${ghc} -fglasgow-exts -H120M ${PARSER_STACK} -Onot -c -o LF_Parser.o -ohi LF_Parser.hi LF_Parser.${PARSER}.hs

#LF_Parser.hi : LF_Parser.${PARSER}.hs ${PARSER_DEPS}
#	# compiling LF_Parser.${PARSER}.hs WITHOUT optimisation, and heap big heap.
#	${ghc} -c -o LF_Parser.hi LF_Parser.${PARSER}.hs 



# additional (final) options
HC_OPTS_Main     += -fglasgow-exts
HC_OPTS_Input    += -fglasgow-exts
HC_OPTS_Signals  += -Onot -fglasgow-exts
HC_OPTS_Printing += -Onot

ghc-hooks.o : ghc-hooks.c
	${ghc} -c ghc-hooks.c

DTD_Ulpip.hs : ulpip.dtd
	/users/haskell/HaXml-1.04/DtdToHaskell ulpip.dtd DTD_Ulpip.hs



################################################################################
# other dependencies

test : all
	rm -f test.log
	pty_driver ${PLASTIC_TEST}/bin/run_tests ./lf | tee tests.log

mk_deps = ${ghc} -cpp -M

hugs_lib = /home/rosedale/package/hugs/current/share/hugs/lib/hugs
hugs_ext = /home/rosedale/package/hugs/current/share/hugs/lib/exts

deps : 
	# echo only on solaris machines.
	cp -p Makefile /tmp/Makefile."${shell date}"
	# copy in deps from hugs lib - else mkdependHS has problems...
	- ln -s ${hugs_lib}/List.hs ${hugs_lib}/Maybe.hs ${hugs_lib}/Monad.hs . 
	- ln -s ${hugs_lib}/System.hs ${hugs_lib}/Trace.hs .

	${mk_deps} *.lhs *.ly

	# remove linked deps.
	rm `find . -type l -name "*hs" -print` 

	../bin/tidyMakefile Makefile && rm Makefile.bak


user_docs : ../doc/user.ps 
	(cd ../doc ; make)

make_PRODUCTION : 
	@# make with traces off
	@# OTHER files need to be recompiled - a grep expression? 
	# DISABLED rm -f DebugOptions.o
	# DISABLED ${ghc} -DPRODUCTION -c DebugOptions.lhs
	${MAKE} all

Build.Makefile :
	${EDIT} Build.Makefile

build_dir : Build.Makefile
	- ln -s ${PLASTIC_BASE}/src/*hs .
	- ln -s ${PLASTIC_BASE}/src/*.c .
	- ln -s ${PLASTIC_BASE}/src/LF_Parser.ly .
	- ln -s ${PLASTIC_BASE}/src/Makefile .
	- ln -s ${PLASTIC_BASE}/src/Local.Makefile .


edit-release-info : 
	- vi +G Main.lhs

bf : lf@${TAG}
	mv lf@${TAG} ../plastic
	chmod 711 ../plastic
	strip ../plastic
	(cd ../doc && cvs up && make user.ps)
	(cd ../lib && cvs up)
	(cd ../test && cvs up)

lx : lf@${TAG}
	- mkdir public_html
	cp lf@${TAG} public_html/plastic-linux
	chmod 755 public_html/plastic-linux
	gzip public_html/plastic-linux
	tar cf - public_html/plastic-linux.gz | rsh ws-pcc tar xf -
	rsh ws-pcc chmod 755 public_html/plastic-linux.gz
	- rm -f public_html

dist : user_docs edit-release-info 
	@ echo Remaking, to include release notes and handle -DPRODUCTION
	${MAKE} make_PRODUCTION

	cp -p ./lf@${TAG} ./plastic
	strip plastic

	rm -f /tmp/plastic-distrib.tgz
	tar chvf - \
			./plastic \
			-C ${PLASTIC_LIB}/..  ${shell cd ${PLASTIC_LIB}/.. && find lib -name '*.lf' -print} \
			-C ${PLASTIC_TEST}/.. `basename ${PLASTIC_TEST}` \
			-C ${PLASTIC_TEST}/.. doc/user.ps doc/distrib.README doc/bugs \
			| gzip -c > /tmp/plastic-distrib.tgz
	chmod 644 /tmp/plastic-distrib.tgz
	rm -f plastic

	# clean up and reinstate traces 
	rm -f DebugOptions.o
	${MAKE} DebugOptions.o 	# doesn't like 2 targets...
	${MAKE} 


graph : Makefile.gra 
	/users/dcs8ttg/OTHER/bin/daVinci -startappl ${PWD} '../bin/daVinci-friend.pl Makefile.gra'

Makefile.gra : Makefile DependencyGraph.lhs DaVinciInterface.lhs
	/users/hugs/current/bin/runhugs ./DependencyGraph.lhs


clean :
	rm -f *.o *.hi lf lf@${TAG} Makefile.gra LF_Parser.${PARSER}.hs 

################################################################################
# the dependencies

# DO NOT DELETE: Beginning of Haskell dependencies
PrettyAux.o : PrettyAux.lhs
IOExts_.o : IOExts_.lhs
Hugs_FiniteMap.o : Hugs_FiniteMap.lhs
BaseClasses.o : BaseClasses.lhs
BaseFunctions.o : BaseFunctions.lhs
BaseFunctions.o : BaseClasses.hi
BaseFunctions.o : IOExts_.hi
BaseTypes.o : BaseTypes.lhs
BaseTypes.o : BaseClasses.hi
DaVinciInterface.o : DaVinciInterface.lhs
DaVinciInterface.o : BaseClasses.hi
ReaderMonads.o : ReaderMonads.lhs
ReaderMonads.o : BaseClasses.hi
ReaderMonads.o : BaseTypes.hi
SharedSyntax.o : SharedSyntax.lhs
SharedSyntax.o : BaseTypes.hi
SharedSyntax.o : BaseClasses.hi
StateMonads.o : StateMonads.lhs
StateMonads.o : BaseClasses.hi
StateMonads.o : BaseTypes.hi
StateMonads.o : BaseFunctions.hi
Base.o : Base.lhs
Base.o : StateMonads.hi
Base.o : ReaderMonads.hi
Base.o : BaseFunctions.hi
Base.o : BaseClasses.hi
Base.o : BaseTypes.hi
DebugOptions.o : DebugOptions.lhs
DebugOptions.o : Base.hi
DebugOptions.o : BaseFunctions.hi
DebugOptions.o : IOExts_.hi
DebugOptions.o : IOExts_.hi
DebugOptions.o : IOExts_.hi
ParsingAux.o : ParsingAux.lhs
ParsingAux.o : SharedSyntax.hi
ParsingAux.o : Base.hi
SimpleTerm.o : SimpleTerm.lhs
SimpleTerm.o : SharedSyntax.hi
SimpleTerm.o : Base.hi
CommandTypes.o : CommandTypes.lhs
CommandTypes.o : SharedSyntax.hi
CommandTypes.o : SimpleTerm.hi
Case.o : Case.lhs
Case.o : SimpleTerm.hi
Case.o : CommandTypes.hi
Case.o : Base.hi
InductiveAux.o : InductiveAux.lhs
InductiveAux.o : DebugOptions.hi
InductiveAux.o : CommandTypes.hi
InductiveAux.o : SharedSyntax.hi
InductiveAux.o : SimpleTerm.hi
InductiveAux.o : Base.hi
InductiveAux.o : IOExts_.hi
Terms.o : Terms.lhs
Terms.o : SharedSyntax.hi
Terms.o : Base.hi
TermReduction.o : TermReduction.lhs
TermReduction.o : Terms.hi
TermReduction.o : SharedSyntax.hi
TermReduction.o : Terms.hi
TermReduction.o : Base.hi
TermReduction.o : DebugOptions.hi
TermReduction.o : IOExts_.hi
TermOps.o : TermOps.lhs
TermOps.o : TermReduction.hi
TermOps.o : TermReduction.hi
TermOps.o : SharedSyntax.hi
TermOps.o : Terms.hi
TermOps.o : Terms.hi
TermOps.o : Base.hi
TermOps.o : IOExts_.hi
Context.o : Context.lhs
Context.o : SharedSyntax.hi
Context.o : TermOps.hi
Context.o : Terms.hi
Context.o : Base.hi
Context.o : IOExts_.hi
Context.o : Hugs_FiniteMap.hi
UserSyntax.o : UserSyntax.lhs
UserSyntax.o : SharedSyntax.hi
UserSyntax.o : Terms.hi
UserSyntax.o : SimpleTerm.hi
UserSyntax.o : Base.hi
UserSyntax.o : IOExts_.hi
GlobalOptions.o : GlobalOptions.lhs
GlobalOptions.o : PrettyAux.hi
GlobalOptions.o : Base.hi
GlobalOptions.o : StateMonads.hi
GlobalOptions.o : IOExts_.hi
GlobalOptions.o : IOExts_.hi
Input.o : Input.lhs
Input.o : GlobalOptions.hi
Input.o : Base.hi
Input.o : IOExts_.hi
PrintingAux.o : PrintingAux.lhs
PrintingAux.o : GlobalOptions.hi
PrintingAux.o : Context.hi
PrintingAux.o : Context.hi
PrintingAux.o : TermOps.hi
PrintingAux.o : SharedSyntax.hi
PrintingAux.o : Terms.hi
PrintingAux.o : ReaderMonads.hi
PrintingAux.o : Base.hi
PrintingAux.o : PrettyAux.hi
PrintingAux.o : IOExts_.hi
Printing.o : Printing.lhs
Printing.o : GlobalOptions.hi
Printing.o : PrintingAux.hi
Printing.o : ReaderMonads.hi
Printing.o : Context.hi
Printing.o : TermReduction.hi
Printing.o : TermOps.hi
Printing.o : Terms.hi
Printing.o : Base.hi
Printing.o : PrettyAux.hi
Printing.o : IOExts_.hi
Debugging.o : Debugging.lhs
Debugging.o : PrintingAux.hi
Debugging.o : Printing.hi
Debugging.o : Context.hi
Debugging.o : TermOps.hi
Debugging.o : Terms.hi
Debugging.o : Base.hi
Debugging.o : PrettyAux.hi
Debugging.o : IOExts_.hi
Reduction.o : Reduction.lhs
Reduction.o : DebugOptions.hi
Reduction.o : Debugging.hi
Reduction.o : SimpleTerm.hi
Reduction.o : TermReduction.hi
Reduction.o : TermReduction.hi
Reduction.o : TermReduction.hi
Reduction.o : TermOps.hi
Reduction.o : Terms.hi
Reduction.o : Context.hi
Reduction.o : Base.hi
Universes.o : Universes.lhs
Universes.o : Debugging.hi
Universes.o : Reduction.hi
Universes.o : SharedSyntax.hi
Universes.o : SharedSyntax.hi
Universes.o : SharedSyntax.hi
Universes.o : Terms.hi
Universes.o : Base.hi
Convertibility.o : Convertibility.lhs
Convertibility.o : DebugOptions.hi
Convertibility.o : Universes.hi
Convertibility.o : PrettyAux.hi
Convertibility.o : Printing.hi
Convertibility.o : Reduction.hi
Convertibility.o : Reduction.hi
Convertibility.o : CommandTypes.hi
Convertibility.o : TermOps.hi
Convertibility.o : SharedSyntax.hi
Convertibility.o : Terms.hi
Convertibility.o : Context.hi
Convertibility.o : Base.hi
ConstraintTypeInf.o : ConstraintTypeInf.lhs
ConstraintTypeInf.o : Convertibility.hi
ConstraintTypeInf.o : Convertibility.hi
ConstraintTypeInf.o : Reduction.hi
ConstraintTypeInf.o : Reduction.hi
ConstraintTypeInf.o : PrettyAux.hi
ConstraintTypeInf.o : Printing.hi
ConstraintTypeInf.o : Context.hi
ConstraintTypeInf.o : TermReduction.hi
ConstraintTypeInf.o : TermReduction.hi
ConstraintTypeInf.o : TermOps.hi
ConstraintTypeInf.o : TermOps.hi
ConstraintTypeInf.o : SharedSyntax.hi
ConstraintTypeInf.o : Terms.hi
ConstraintTypeInf.o : GlobalOptions.hi
ConstraintTypeInf.o : DebugOptions.hi
ConstraintTypeInf.o : Debugging.hi
ConstraintTypeInf.o : Base.hi
ConstraintTypeInf.o : BaseFunctions.hi
ConstraintTypeInf.o : BaseTypes.hi
ConstraintTypeInf.o : BaseClasses.hi
ConstraintTypeInf.o : IOExts_.hi
Coercions.o : Coercions.lhs
Coercions.o : Reduction.hi
Coercions.o : ConstraintTypeInf.hi
Coercions.o : Convertibility.hi
Coercions.o : Convertibility.hi
Coercions.o : Convertibility.hi
Coercions.o : TermReduction.hi
Coercions.o : Context.hi
Coercions.o : Terms.hi
Coercions.o : Base.hi
Coercions.o : Debugging.hi
TypeInference.o : TypeInference.lhs
TypeInference.o : PrettyAux.hi
TypeInference.o : Printing.hi
TypeInference.o : GlobalOptions.hi
TypeInference.o : DebugOptions.hi
TypeInference.o : Coercions.hi
TypeInference.o : Reduction.hi
TypeInference.o : Convertibility.hi
TypeInference.o : Context.hi
TypeInference.o : TermReduction.hi
TypeInference.o : TermOps.hi
TypeInference.o : SharedSyntax.hi
TypeInference.o : Terms.hi
TypeInference.o : Base.hi
TypeInference.o : IOExts_.hi
ContextOps.o : ContextOps.lhs
ContextOps.o : TypeInference.hi
ContextOps.o : Universes.hi
ContextOps.o : SharedSyntax.hi
ContextOps.o : StateMonads.hi
ContextOps.o : Printing.hi
ContextOps.o : CommandTypes.hi
ContextOps.o : Context.hi
ContextOps.o : TermReduction.hi
ContextOps.o : TermOps.hi
ContextOps.o : Terms.hi
ContextOps.o : Base.hi
ContextOps.o : IOExts_.hi
SimpleToTerm.o : SimpleToTerm.lhs
SimpleToTerm.o : ContextOps.hi
SimpleToTerm.o : Case.hi
SimpleToTerm.o : SharedSyntax.hi
SimpleToTerm.o : Context.hi
SimpleToTerm.o : TermOps.hi
SimpleToTerm.o : Terms.hi
SimpleToTerm.o : SimpleTerm.hi
SimpleToTerm.o : Base.hi
SimpleToTerm.o : IOExts_.hi
CoercionCmds.o : CoercionCmds.lhs
CoercionCmds.o : SharedSyntax.hi
CoercionCmds.o : ConstraintTypeInf.hi
CoercionCmds.o : ConstraintTypeInf.hi
CoercionCmds.o : Convertibility.hi
CoercionCmds.o : Reduction.hi
CoercionCmds.o : TypeInference.hi
CoercionCmds.o : Printing.hi
CoercionCmds.o : TermReduction.hi
CoercionCmds.o : TermOps.hi
CoercionCmds.o : Terms.hi
CoercionCmds.o : Terms.hi
CoercionCmds.o : Context.hi
CoercionCmds.o : SimpleToTerm.hi
CoercionCmds.o : SimpleTerm.hi
CoercionCmds.o : Debugging.hi
CoercionCmds.o : DebugOptions.hi
CoercionCmds.o : GlobalOptions.hi
CoercionCmds.o : CommandTypes.hi
CoercionCmds.o : StateMonads.hi
CoercionCmds.o : Base.hi
CoercionCmds.o : BaseTypes.hi
CoercionCmds.o : BaseClasses.hi
CoercionCmds.o : BaseClasses.hi
CoercionCmds.o : PrettyAux.hi
ComputationRules.o : ComputationRules.lhs
ComputationRules.o : TypeInference.hi
ComputationRules.o : SimpleToTerm.hi
ComputationRules.o : SimpleTerm.hi
ComputationRules.o : GlobalOptions.hi
ComputationRules.o : DebugOptions.hi
ComputationRules.o : Debugging.hi
ComputationRules.o : Printing.hi
ComputationRules.o : CommandTypes.hi
ComputationRules.o : ContextOps.hi
ComputationRules.o : Context.hi
ComputationRules.o : TermReduction.hi
ComputationRules.o : TermOps.hi
ComputationRules.o : Terms.hi
ComputationRules.o : Base.hi
IntrosReturn.o : IntrosReturn.lhs
IntrosReturn.o : SimpleToTerm.hi
IntrosReturn.o : SimpleTerm.hi
IntrosReturn.o : GlobalOptions.hi
IntrosReturn.o : Debugging.hi
IntrosReturn.o : Printing.hi
IntrosReturn.o : ContextOps.hi
IntrosReturn.o : Context.hi
IntrosReturn.o : TermReduction.hi
IntrosReturn.o : TermOps.hi
IntrosReturn.o : SharedSyntax.hi
IntrosReturn.o : Terms.hi
IntrosReturn.o : Base.hi
MetaVars.o : MetaVars.lhs
MetaVars.o : ConstraintTypeInf.hi
MetaVars.o : TypeInference.hi
MetaVars.o : Reduction.hi
MetaVars.o : Printing.hi
MetaVars.o : SimpleToTerm.hi
MetaVars.o : ContextOps.hi
MetaVars.o : Context.hi
MetaVars.o : SharedSyntax.hi
MetaVars.o : TermOps.hi
MetaVars.o : Terms.hi
MetaVars.o : DebugOptions.hi
MetaVars.o : PrettyAux.hi
MetaVars.o : StateMonads.hi
MetaVars.o : Base.hi
MetaVars.o : BaseClasses.hi
MetaVars.o : BaseClasses.hi
ContextCmds.o : ContextCmds.lhs
ContextCmds.o : MetaVars.hi
ContextCmds.o : Convertibility.hi
ContextCmds.o : TypeInference.hi
ContextCmds.o : Reduction.hi
ContextCmds.o : SimpleToTerm.hi
ContextCmds.o : SharedSyntax.hi
ContextCmds.o : SimpleTerm.hi
ContextCmds.o : GlobalOptions.hi
ContextCmds.o : Debugging.hi
ContextCmds.o : PrettyAux.hi
ContextCmds.o : Printing.hi
ContextCmds.o : CommandTypes.hi
ContextCmds.o : ContextOps.hi
ContextCmds.o : Context.hi
ContextCmds.o : TermOps.hi
ContextCmds.o : Terms.hi
ContextCmds.o : Base.hi
Inductive.o : Inductive.lhs
Inductive.o : ComputationRules.hi
Inductive.o : Debugging.hi
Inductive.o : InductiveAux.hi
Inductive.o : InductiveAux.hi
Inductive.o : CommandTypes.hi
Inductive.o : CommandTypes.hi
Inductive.o : Printing.hi
Inductive.o : ContextCmds.hi
Inductive.o : Context.hi
Inductive.o : TermReduction.hi
Inductive.o : TermOps.hi
Inductive.o : SharedSyntax.hi
Inductive.o : Terms.hi
Inductive.o : SimpleToTerm.hi
Inductive.o : SimpleTerm.hi
Inductive.o : Base.hi
Refine.o : Refine.lhs
Refine.o : MetaVars.hi
Refine.o : Reduction.hi
Refine.o : TypeInference.hi
Refine.o : Convertibility.hi
Refine.o : GlobalOptions.hi
Refine.o : Debugging.hi
Refine.o : Printing.hi
Refine.o : ContextOps.hi
Refine.o : SimpleToTerm.hi
Refine.o : Context.hi
Refine.o : Terms.hi
Refine.o : Base.hi
Tactics.o : Tactics.lhs
Tactics.o : SharedSyntax.hi
Tactics.o : MetaVars.hi
Tactics.o : Refine.hi
Tactics.o : InductiveAux.hi
Tactics.o : Reduction.hi
Tactics.o : TypeInference.hi
Tactics.o : Convertibility.hi
Tactics.o : GlobalOptions.hi
Tactics.o : Debugging.hi
Tactics.o : ContextOps.hi
Tactics.o : SimpleToTerm.hi
Tactics.o : IntrosReturn.hi
Tactics.o : Context.hi
Tactics.o : SimpleTerm.hi
Tactics.o : TermOps.hi
Tactics.o : Terms.hi
Tactics.o : Base.hi
Tactics.o : IOExts_.hi
Signals.o : Signals.lhs
Signals.o : Base.hi
Signals.o : Context.hi
Signals.o : StateMonads.hi
Signals.o : IOExts_.hi
Signals.o : IOExts_.hi
UniverseCmds.o : UniverseCmds.lhs
UniverseCmds.o : Reduction.hi
UniverseCmds.o : Universes.hi
UniverseCmds.o : TypeInference.hi
UniverseCmds.o : SimpleToTerm.hi
UniverseCmds.o : SharedSyntax.hi
UniverseCmds.o : SimpleTerm.hi
UniverseCmds.o : ContextCmds.hi
UniverseCmds.o : ContextOps.hi
UniverseCmds.o : Context.hi
UniverseCmds.o : TermReduction.hi
UniverseCmds.o : TermOps.hi
UniverseCmds.o : Terms.hi
UniverseCmds.o : SharedSyntax.hi
UniverseCmds.o : Terms.hi
UniverseCmds.o : CommandTypes.hi
UniverseCmds.o : Debugging.hi
UniverseCmds.o : StateMonads.hi
UniverseCmds.o : Base.hi
UniverseCmds.o : BaseClasses.hi
./LF_Parser.o : ./LF_Parser.hs
./LF_Parser.o : UserSyntax.hi
./LF_Parser.o : SharedSyntax.hi
./LF_Parser.o : SimpleTerm.hi
./LF_Parser.o : CommandTypes.hi
./LF_Parser.o : ParsingAux.hi
./LF_Parser.o : BaseFunctions.hi
./LF_Parser.o : BaseTypes.hi
./LF_Parser.o : BaseClasses.hi
./LF_Parser.o : IOExts_.hi
Plastic.o : Plastic.lhs
Plastic.o : DebugOptions.hi
Plastic.o : DebugOptions.hi
Plastic.o : Debugging.hi
Plastic.o : Signals.hi
Plastic.o : GlobalOptions.hi
Plastic.o : GlobalOptions.hi
Plastic.o : GlobalOptions.hi
Plastic.o : Input.hi
Plastic.o : UserSyntax.hi
Plastic.o : MetaVars.hi
Plastic.o : CoercionCmds.hi
Plastic.o : Case.hi
Plastic.o : Printing.hi
Plastic.o : Tactics.hi
Plastic.o : IntrosReturn.hi
Plastic.o : Refine.hi
Plastic.o : Inductive.hi
Plastic.o : ComputationRules.hi
Plastic.o : Convertibility.hi
Plastic.o : TypeInference.hi
Plastic.o : Reduction.hi
Plastic.o : UniverseCmds.hi
Plastic.o : ContextOps.hi
Plastic.o : ContextOps.hi
Plastic.o : ContextCmds.hi
Plastic.o : Context.hi
Plastic.o : SimpleToTerm.hi
Plastic.o : TermReduction.hi
Plastic.o : Terms.hi
Plastic.o : ./LF_Parser.hi
Plastic.o : CommandTypes.hi
Plastic.o : SimpleTerm.hi
Plastic.o : Base.hi
Plastic.o : IOExts_.hi
Plastic.o : LF_Parser.hi
Main.o : Main.lhs
Main.o : Plastic.hi
TestParser.o : TestParser.lhs
TestParser.o : CommandTypes.hi
TestParser.o : ./LF_Parser.hi
TestParser.o : Base.hi
# DO NOT DELETE: End of Haskell dependencies
