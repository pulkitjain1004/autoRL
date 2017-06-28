#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void fdouble_metaphone(char**, char**, char**, int*);

static const R_CMethodDef CEntries[] = {
  {"fdouble_metaphone", (DL_FUNC) &fdouble_metaphone, 4},
  {NULL, NULL, 0}
};

void R_init_PGRdup(DllInfo *dll)
{
  R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
