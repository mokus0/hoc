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

struct hoc_method {
    SEL method_name;
    char *method_types;
    IMP method_imp;
};

struct hoc_method_list {
    int method_count;
    
    /* variable length structure */
    struct hoc_method method_list[1];
};

struct hoc_method_list * makeMethodList(int n);

void setMethodInListWithIMP(
        struct hoc_method_list *list,
        int i,
        SEL sel,
        char *types,    /* never deallocate this */
        IMP imp         /* never deallocate this */
    );

void setMethodInList(
        struct hoc_method_list *list,
        int i,
        SEL sel,
        char *types,    /* never deallocate this */
        ffi_cif *cif,   /* never deallocate this */
        haskellIMP imp
    );

#endif /* __Methods_h__ */
