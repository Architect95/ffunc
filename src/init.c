#include "ffunc.h"

static const R_CallMethodDef CallEntries[] = {
  {"CfsubstrR",       (DL_FUNC) &fsubstrR,          -1},
  {"CfsubstrassignR", (DL_FUNC) &fsubstrassignR,    -1},
  {NULL,              NULL,                         -1}
};

void R_init_ffunc(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_RegisterCCallable("ffunc", "CfsubstrR",       (DL_FUNC) &fsubstrR);
  R_RegisterCCallable("ffunc", "CfsubstrassignR", (DL_FUNC) &fsubstrassignR);

}
