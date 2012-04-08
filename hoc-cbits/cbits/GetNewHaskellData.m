#include <stdlib.h>

#include "Common.h"
#include "Class.h"
#include "GetNewHaskellData.h"

/*
    Why don't we use normal Objective-C messaging?
    Because the object in question might not implement __getHaskellData__.
    
    Why not add __getHaskellData__ as a category to NSObject or use
    respondsToSelector?
    Because the object might not even descend from NSObject.
*/
    
typedef HsStablePtr (*getHaskellDataIMP)(id target, SEL sel);

static SEL selGetHaskellData = 0;

// Like [obj __getHaskellData__] but _only_ searching the given
// class for an implementation ('obj' should be an instance of 
// 'isa')
HsStablePtr getNewHaskellDataForClass(id obj, Class isa)
{
    #if DO_LOG
    printf("getNewHaskellDataForClass(%p, %s)\n",
            (void *) obj, isa ? class_getName(isa) : "<null>");
    #endif
    
    struct objc_method *m;

    IMP imp = 0;

    if(!isa)
        return 0;

    if(!selGetHaskellData)
        selGetHaskellData = sel_registerName("__getHaskellData__");
    
    m = class_getInstanceMethod(isa, selGetHaskellData);

    if(m)
        imp = method_getImplementation(m);
    
    #if DO_LOG
    printf("getNewHaskellDataForClass: m = %p, imp = %p\n", m, imp);
    #endif
    
    if(imp)
        return (*(getHaskellDataIMP)imp)(obj, selGetHaskellData);
    else
        return 0;
}


// Like [obj __getHaskellData__] but _only_ searching the object's
// most-specific class for an implementation.
HsStablePtr getNewHaskellData(id obj)
{
    #if DO_LOG
    printf("getNewHaskellData(%p)\n", (void *) obj);
    #endif
    
    return getNewHaskellDataForClass(obj, object_getClass(obj));
}
