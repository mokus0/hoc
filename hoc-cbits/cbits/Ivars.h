#ifndef __Ivars_h__
#define __Ivars_h__

#include <stdlib.h>
#include <stdint.h>

#include "Common.h"

#ifdef GNUSTEP
struct objc_ivar *
object_getInstanceVariable(id obj, const char *name, void** out);

struct objc_ivar *
object_setInstanceVariable(id obj, const char *name, void* val);
#endif

struct hoc_ivar {
    char *ivar_name;
    char *ivar_types;
    size_t ivar_size;
    uint8_t ivar_alignment;
};

#define IVAR_PTR_ALIGN              ((uint8_t) sizeof(void *))

struct hoc_ivar_list {
    int ivar_count;
    
    /* variable length structure */
    struct hoc_ivar ivar_list[1];
};

struct hoc_ivar_list * makeIvarList(int n);

void setIvarInList(
        struct hoc_ivar_list *list,
        int i,
        char *name,     /* never deallocate this */
        char *types,    /* never deallocate this */
        size_t size,
        uint8_t alignment
    );

#endif /* __Ivars_h__ */
