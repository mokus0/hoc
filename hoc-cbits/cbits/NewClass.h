#ifndef __NewClass_h__
#define __NewClass_h__

#include "Common.h"

struct hoc_ivar_list;
struct hoc_method_list;

void newClass(Class super_class,
                const char * name,                          /* never deallocate this */
				struct hoc_ivar_list *ivars,
				struct hoc_method_list *methods,
				struct hoc_method_list *class_methods);
				

#endif /* __NewClass_h__ */
