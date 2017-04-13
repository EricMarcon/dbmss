#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP dbmss_CountNbdKd(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP dbmss_DistKd(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP dbmss_parallelCountNbd(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP dbmss_parallelCountNbdCC(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP dbmss_parallelCountNbdDt(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP dbmss_parallelCountNbdDtCC(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP dbmss_parallelCountNbdm(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"dbmss_CountNbdKd",           (DL_FUNC) &dbmss_CountNbdKd,           7},
    {"dbmss_DistKd",               (DL_FUNC) &dbmss_DistKd,               7},
    {"dbmss_parallelCountNbd",     (DL_FUNC) &dbmss_parallelCountNbd,     6},
    {"dbmss_parallelCountNbdCC",   (DL_FUNC) &dbmss_parallelCountNbdCC,   6},
    {"dbmss_parallelCountNbdDt",   (DL_FUNC) &dbmss_parallelCountNbdDt,   5},
    {"dbmss_parallelCountNbdDtCC", (DL_FUNC) &dbmss_parallelCountNbdDtCC, 5},
    {"dbmss_parallelCountNbdm",    (DL_FUNC) &dbmss_parallelCountNbdm,    3},
    {NULL, NULL, 0}
};

void R_init_dbmss(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, TRUE);
}
