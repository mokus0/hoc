#include <stdlib.h>
#include <assert.h>

#include "Common.h"
#include "Ivars.h"

#ifdef GNUSTEP

struct objc_ivar *
object_getInstanceVariable(id obj, const char *name, void** out)
{
    struct objc_ivar *ivar = class_getInstanceVariable(obj->class_pointer,name);
    if(ivar)
        *out = *(void**) ((char*)obj + ivar->ivar_offset);
    return ivar;
}

struct objc_ivar *
object_setInstanceVariable(id obj, const char *name, void* val)
{
    struct objc_ivar *ivar = class_getInstanceVariable(obj->class_pointer,name);
    if(ivar)
        *(void**) ((char*)obj + ivar->ivar_offset) = val;
    return ivar;
}
#endif


struct hoc_ivar_list * makeIvarList(int n)
{
    struct hoc_ivar_list *list = 
        calloc(1, sizeof(struct hoc_ivar_list)
                  + (n-1) * sizeof(struct hoc_ivar));
    list->ivar_count = n;
    return list;
}

void setIvarInList(
        struct hoc_ivar_list *list,
        int i,
        char *name,
        char *types,
        size_t size,
        uint8_t alignment
    )
{
    list->ivar_list[i].ivar_name      = name;
    list->ivar_list[i].ivar_types     = types;
    list->ivar_list[i].ivar_size      = size;
    list->ivar_list[i].ivar_alignment = alignment;
}
