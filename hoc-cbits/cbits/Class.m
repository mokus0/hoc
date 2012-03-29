#include "Common.h"

Class getSuperClassForObject(id self)
{
    return object_getClass(self)->super_class;
}
