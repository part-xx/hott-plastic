
#define MSG "\nFAIL[CRASH]: "


#if __GLASGOW_HASKELL__ < 400

#include <rtsdefs.h>

void defaultsHook (void) {
	RTSflags.GcFlags.stksSize =  4000002 / sizeof(W_);	/* 4M */
	RTSflags.GcFlags.heapSize =  20000002 / sizeof(W_);	/* 20M */
}  
		/* these aren't meaningful in ghc-4.01 */

void
ErrorHdrHook (FILE *where)
{
	fflush( stdout );                   /* Flush out any pending output */
	fprintf(where, MSG);
}

#else 

#include <stdio.h>
#include <string.h>
/* #include <Hooks.h> */
	/* header for the following */

void
ErrorHdrHook (long fd) 
{
	write(fd, MSG, strlen(MSG));
}

#endif

