#include <stdlib.h>
#include <Foundation/NSException.h>
#include <assert.h>
#include "Class.h"
#include "Ivars.h"
#include "Log.h"
#include "Methods.h"
#include "NewClass.h"

static void addIvarsToClass(Class new_class, struct hoc_ivar_list *ivars)
{
    int i;
    
    for (i = 0; i < ivars->ivar_count; i++)
    {
        struct hoc_ivar *ivar = &ivars->ivar_list[i];
        
        #if DO_LOG
        printf("adding ivar (%s, %s, %d, %d) to class %s\n", 
            ivar->ivar_name, ivar->ivar_types, (int) ivar->ivar_size, (int) ivar->ivar_alignment, class_getName(new_class));
        #endif
        
        class_addIvar(new_class, ivar->ivar_name,
            ivar->ivar_size, ivar->ivar_alignment, ivar->ivar_types);
    }
}

static void addMethodsToClass(Class new_class, struct hoc_method_list *methods)
{
    int i;
    for (i = 0; i < methods->method_count; i++)
    {
        struct hoc_method * m = &methods->method_list[i];
        
        #if DO_LOG
        printf("adding method (%s, %s, %p) to class %s\n", 
            sel_getName(m->method_name), m->method_types, m->method_imp, class_getName(new_class));
        #endif
        
        class_addMethod(new_class, m->method_name, m->method_imp, m->method_types);
    }
}

void newClass(Class super_class,
                const char * name,
                struct hoc_ivar_list *ivars,
                struct hoc_method_list *methods,
                struct hoc_method_list *class_methods)
{
	Class meta_class;
	Class new_class;
	
	assert(objc_lookUpClass(name) == nil);
	
    #if DO_LOG
    printf("newClass(%s, %d ivars, %d methods, %d class methods)\n", name, 
        ivars->ivar_count, methods->method_count, class_methods->method_count);
    #endif
    
	/* Allocate the class and metaclass */
    new_class = objc_allocateClassPair(super_class, name, 0);
    meta_class = object_getClass(new_class);
	
    #if DO_LOG
    printf("    new_class = %p, meta_class = %p\n", new_class, meta_class);
    #endif
    
	/* Add instance variables to the class */
    addIvarsToClass(new_class, ivars);
    
    /* Add methods and class methods */
    addMethodsToClass(new_class, methods);
    addMethodsToClass(meta_class, class_methods);
    
    objc_registerClassPair(new_class);
}

