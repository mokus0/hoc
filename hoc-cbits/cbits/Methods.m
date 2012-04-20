#include "Methods.h"
#include "Statistics.h"
#include <hs_libffi_closure.h>
#include <stdlib.h>
#include <sys/mman.h>
#import <Foundation/NSException.h>

static void objcIMP(ffi_cif *cif, void * ret, void **args, void *userData)
{
    #if DO_LOG
    printf("objcIMP(%p, %p, %p, %p)\n", cif, ret, args, userData);
    #endif
    
    recordHOCEvent(kHOCAboutToEnterHaskell, args);
    NSException *e = (*(haskellIMP)userData)(cif, ret, args);
    recordHOCEvent(kHOCLeftHaskell, args);
    if(e != nil)
        [e raise];
}

IMP newIMP(ffi_cif *cif, haskellIMP imp)
{
    void *entry;
    ffi_closure *closure = hs_ffi_closure_alloc(sizeof(ffi_closure), &entry);
    
    #if DO_LOG
    printf("newIMP(%p, %p) - closure = %p, entry = %p\n", cif, imp, closure, entry);
    #endif
    
    ffi_prep_closure_loc(closure, cif, &objcIMP, (void*) imp, entry);
    return (IMP) entry;
}
