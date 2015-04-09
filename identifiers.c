#include <stdio.h>
void print_info () {
	puts("BUILD DATE = Sun  6 Jul 23:12:51 BST 2014");
	puts("BUILD INFO = DPRODUCTION,D__HASKELL98__");
	puts("COMPILER   = ghc -cpp -package ghc-7.6.3 -package base -hide-package haskell98 -cpp -H30M  -dshow-passes  -fwarn-incomplete-patterns -fwarn-unused-binds -DPRODUCTION -D__HASKELL98__  ");
	fflush(stdout);
}
