#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP _dbmss_CountNbdKd(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _dbmss_DistKd(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _dbmss_parallelCountNbd(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _dbmss_parallelCountNbdCC(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _dbmss_parallelCountNbdDt(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _dbmss_parallelCountNbdDtCC(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _dbmss_parallelCountNbdm(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_dbmss_CountNbdKd",           (DL_FUNC) &_dbmss_CountNbdKd,           7},
    {"_dbmss_DistKd",               (DL_FUNC) &_dbmss_DistKd,               7},
    {"_dbmss_parallelCountNbd",     (DL_FUNC) &_dbmss_parallelCountNbd,     6},
    {"_dbmss_parallelCountNbdCC",   (DL_FUNC) &_dbmss_parallelCountNbdCC,   6},
    {"_dbmss_parallelCountNbdDt",   (DL_FUNC) &_dbmss_parallelCountNbdDt,   5},
    {"_dbmss_parallelCountNbdDtCC", (DL_FUNC) &_dbmss_parallelCountNbdDtCC, 5},
    {"_dbmss_parallelCountNbdm",    (DL_FUNC) &_dbmss_parallelCountNbdm,    3},
    {NULL, NULL, 0}
};

void R_init_dbmss(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, TRUE);
}
