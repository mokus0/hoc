#ifndef __FFICallInterface_h__
#define __FFICallInterface_h__

#include <ffi.h>

ffi_cif * allocCif();
ffi_abi defaultABI();
ffi_type * allocStructType(ffi_type **elements);

int cifIsStret(ffi_cif *cif);

#endif /* __FFICallInterface_h__ */
