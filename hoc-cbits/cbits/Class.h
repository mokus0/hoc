#ifndef __Class_h__
#define __Class_h__

#include "Common.h"

id getClassByName(const char* name);

Class getSuperclassForClass(Class class);
Class getRootClassForClass(Class super_class);

Class getClassForObject(id self);
Class getSuperClassForObject(id self);

#endif /* __Class_h__ */
