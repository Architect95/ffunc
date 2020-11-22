
#include "ffunc_utils.h"

// The following min/max functions are credited to Sean Eron Anderson et al.
// Source: http://graphics.stanford.edu/~seander/bithacks.html#IntegerMinOrMax

int min(int a, int b) { return b ^ ((a ^ b) & -(a < b)); }
int max(int a, int b) { return a ^ ((a ^ b) & -(a < b)); }

// If you know that INT_MIN <= a - b <= INT_MAX, you can use:
int unguardedMin(int a, int b) { 
    return b + ((a - b) & ((a - b) >> (sizeof(int) * CHAR_BIT - 1)));
}
int unguardedMax(int a, int b) { 
    return a - ((a - b) & ((a - b) >> (sizeof(int) * CHAR_BIT - 1)));
}

// End