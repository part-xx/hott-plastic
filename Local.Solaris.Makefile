TIME=
PERL=perl 
MAKE=make
PERL=perl5
HAPPY=/users/haskell/happy-1.6/bin/happy
HAPPY=/users/haskell/happy-1.8/bin/happy


################################################################################
# local settings for main directory location etc

PLASTIC_BASE=..

################################################################################
# local settings for ghc locations.

# base dir of ghc installations (old)
GHC_BASE=/home/rosedale/package/haskell

ghc_v2_10 = /net/easby/export/home/package3/lego/ghc-2.10/fptools/bin/sparc-sun-solaris2/ghc-2.10/ghc-2.10 

ghc_v3_02 = \
	/net/easby/export/home/package3/lego/ghc-3.02/build/bin/ghc-3.02 \
		-fshow-import-specs -optC-freport-disallowed-unfoldings

ghc_v4_03 = /net/easby/export/home/package3/lego/ghc-cvs/vanilla/build/bin/ghc-4.03

# ghc_v4_03_DEBUG = /net/easby/export/home/package3/lego/ghc-cvs/rts-debug/build/bin/ghc-4.03 -syslib posix -syslib exts -DV_GHC=403 -optc-DV_GHC=403

ghc_v4_04 = ${GHC_BASE}/ghc-4.04/bin/ghc-4.04

ghc_v4_05 = ${GHC_BASE}/ghc-cvs/ghc-4.05-first/bin/ghc-4.05

ghc_v4_06 = ${GHC_BASE}/ghc-4.06/fptools/bin/sparc-sun-solaris2/ghc-4.06

ghc_v4_08 = ${GHC_BASE}/ghc-4.08/bin/sparc-sun-solaris2/ghc-4.08

ghc_v4_08 = ${GHC_BASE}/ghc-4.08/bin/sparc-sun-solaris2/ghc-4.08


# note: can't use gcc as a linker - has some funny interaction with readline
# but when using gnu-ld, must say explicitly where to find the gcc lib...
ghc_v5_02 = ${GHC_BASE}/ghc-5.02.2/bin/sparc-sun-solaris2/ghc 

# shouldn#t need
# -pgml/usr/local/gnu/bin/ld -optl'-L/usr/local/gnu/lib/gcc-lib/sparc-sun-solaris2.6/2.95.2 -lgcc'


# trying without any fix like the above.
ghc_v5_04 = ${GHC_BASE}/ghc-5.04.1/bin/sparc-sun-solaris2/ghc 
