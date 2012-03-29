#include "Common.h"

Class getSuperClassForObject(id self)
{
    return class_getSuperclass(object_getClass(self));
}
