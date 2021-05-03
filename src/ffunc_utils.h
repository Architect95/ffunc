#ifndef FFUNC_UTILS_H
#define FFUNC_UTILS_H

#include <limits.h>

int   sign(int a);
_Bool oppositeSign(int a, int b);

int min(int a, int b);
int max(int a, int b);
int unguardedMin(int a, int b);
int unguardedMax(int a, int b);

#endif