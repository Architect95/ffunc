
#include "ffunc_utils.h"

// The following functions are credited to Sean Eron Anderson et al.
// Source: http://graphics.stanford.edu/~seander/bithacks.html

inline int sign(int a) {
  return +1 | -(int)((unsigned int)(a) >> (sizeof(int) * CHAR_BIT - 1));
}
inline _Bool oppositeSign(int a, int b) { return (a ^ b) < 0; }

inline int min(int a, int b) { return b ^ ((a ^ b) & -(a < b)); }
inline int max(int a, int b) { return a ^ ((a ^ b) & -(a < b)); }

// If you know that INT_MIN <= a - b <= INT_MAX, you can use:
inline int unguardedMin(int a, int b) { 
  return b + ((a - b) & ((a - b) >> (sizeof(int) * CHAR_BIT - 1)));
}
inline int unguardedMax(int a, int b) { 
  return a - ((a - b) & ((a - b) >> (sizeof(int) * CHAR_BIT - 1)));
}

// End