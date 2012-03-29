#include <stdlib.h>
#include <sys/mman.h>
#include "Methods.h"
#include "Statistics.h"

#ifdef __OBJC__
#import <Foundation/NSException.h>
#endif

static void objcIMP(ffi_cif *cif, void * ret, void **args, void *userData)
{
    recordHOCEvent(kHOCAboutToEnterHaskell, args);
    NSException *e = (*(haskellIMP)userData)(cif, ret, args);
    recordHOCEvent(kHOCLeftHaskell, args);
    if(e != nil)
        [e raise];
}

static ffi_closure *newIMP(ffi_cif *cif, haskellIMP imp)
{
    //ffi_closure *closure = (ffi_closure*) calloc(1, sizeof(ffi_closure));
    ffi_closure *closure = mmap(NULL, sizeof(ffi_closure), PROT_READ | PROT_WRITE | PROT_EXEC, MAP_ANON | MAP_PRIVATE, -1, 0);
    if (closure == (void*)-1)
    {
        // TODO: Check errno and handle the error.
    }

    ffi_prep_closure(closure, cif, &objcIMP, (void*) imp);
    return closure;
}

struct hoc_method_list * makeMethodList(int n)
{
    struct hoc_method_list *list = 
        calloc(1, sizeof(struct hoc_method_list)
                  + (n-1) * sizeof(struct hoc_method));
    list->method_count = n;
    return list;
}

void setMethodInList(
        struct hoc_method_list *list,
        int i,
        SEL sel,
        char *types,
        ffi_cif *cif,
        haskellIMP imp
    )
{   
    setMethodInListWithIMP(list, i, sel, types, (IMP) newIMP(cif, imp) );
}

void setMethodInListWithIMP(
        struct hoc_method_list *list,
        int i,
        SEL sel,
        char *types,
        IMP imp
    )
{
    list->method_list[i].method_name = sel;
    list->method_list[i].method_types = types;
    list->method_list[i].method_imp = imp;
}
