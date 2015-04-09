TIME=
PERL=perl 
MAKE=make
PERL=perl 
HAPPY=/home/dcs0pcc/ghc/happy-1.6/bin/happy
HAPPY=happy


################################################################################
# local settings for main directory location etc

PLASTIC_BASE=..

################################################################################
# local settings for ghc locations.

ghc_v4_06_linux = /home/dcs0pcc/ghc/ghc-4.06/bin/ghc-4.06 

ghc_v4_08_linux = /home/dcs0pcc/ghc/ghc-4.08.1/bin/ghc-4.08.1

ghc_v5_02_linux = /home/dcs0pcc/ghc/ghc-5.02.2/bin/ghc \
		-package posix -package lang -package data -package concurrent\
                -DV_GHC=502 -optc-DV_GHC=502

ghc_v5_04_linux = ghc-5.04.3 \
		-package posix -package lang -package data -package concurrent\
                -DV_GHC=504 -optc-DV_GHC=504

ghc_v6_linux = ghc-6.0
		
