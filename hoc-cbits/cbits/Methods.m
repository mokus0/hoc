#include "Methods.h"
#include "Statistics.h"
#include <hs_libffi_closure.h>
#include <stdlib.h>
#include <sys/mman.h>
#import <Foundation/NSException.h>

static void objcIMP(ffi_cif *cif, void * ret, void **args, void *userData)
{
    recordHOCEvent(kHOCAboutToEnterHaskell, args);
    NSException *e = (*(haskellIMP)userData)(cif, ret, args);
    recordHOCEvent(kHOCLeftHaskell, args);
    if(e != nil)
        [e raise];
}

static IMP newIMP(ffi_cif *cif, haskellIMP imp)
{
    void *entry;
    ffi_closure *closure = ffi_closure_alloc(sizeof(ffi_closure), &entry);
    ffi_prep_closure_loc(closure, cif, &objcIMP, (void*) imp, entry);
    return (IMP) entry;
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
