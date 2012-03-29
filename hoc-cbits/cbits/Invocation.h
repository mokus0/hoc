#ifndef __Invocation_h__
#define __Invocation_h__

#include <ffi.h>

@class NSException;

NSException *callWithExceptions(ffi_cif *cif, void (*fn)(),
                                void *rvalue, void **avalue);

#endif /* __Invocation_h__ */
