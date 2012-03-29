#include <stdlib.h>

#include "Common.h"
#include "Class.h"
#include "GetNewHaskellData.h"

#include "Log.h"

/*
    Why don't we use normal Objective-C messaging?
    Because the object in question might not implement __getHaskellData__.
    
    Why not add __getHaskellData__ as a category to NSObject or use
    respondsToSelector?
    Because the object might not even descend from NSObject.
*/
    
typedef void * (*getHaskellDataIMP)(id target, SEL sel);

static SEL selGetHaskellData = 0;

void *getNewHaskellDataForClass(id obj, Class isa)
{
    #if DO_LOG
    printf("getNewHaskellDataForClass(%p, %p)\n", (void *) obj, (void *) isa);
    #endif
    
    struct objc_method *m;

    IMP imp = 0;

    if(!isa)
        return 0;

    if(!selGetHaskellData)
        selGetHaskellData = sel_registerName("__getHaskellData__");
    
// #ifdef GNUSTEP
//         // first, use objc_msg_lookup to make sure
//         // that the objc runtime has inited everything
//         // TODO: find out whether this is really necessary (it doesn't seem to be), 
//         //  and if so figure out a way to achieve the same goal without spewing 
//         //  warnings all over the console.
//     objc_msg_lookup(obj, selGetHaskellData);
// #endif
    
        // Now find the right method.
        // We don't want to use objc_msg_lookup_super because 
        // we don't want our message to be forwarded.
    m = class_getInstanceMethod(isa, selGetHaskellData);

    if(m)
        imp = method_getImplementation(m);
    
    if(imp)
        return (*(getHaskellDataIMP)imp)(obj, selGetHaskellData);
    else
        return 0;
}


void *getNewHaskellData(id obj)
{
    #if DO_LOG
    printf("getNewHaskellData(%p)\n", (void *) obj);
    #endif
    
    return getNewHaskellDataForClass(obj, object_getClass(obj));
}
