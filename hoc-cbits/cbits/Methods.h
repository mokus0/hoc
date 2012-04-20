#ifndef __Methods_h__
#define __Methods_h__

#include <ffi.h>
#include "Common.h"

@class NSException;

typedef NSException *(*haskellIMP)(
                        ffi_cif *cif,
                        void * ret,
                        void **args
                    );

#endif /* __Methods_h__ */
