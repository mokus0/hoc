#include <stdlib.h>
#include <Foundation/NSAutoreleasePool.h>

#include "Common.h"
#include "Class.h"
#include "Log.h"
#include "MemoryManagement.h"

void retainObject(id obj)
{
	[obj retain];
}

void releaseObject(id obj)
{
	[obj release];
}

void deallocObject(id obj)
{
	[obj dealloc];
}

void autoreleaseObject(id obj)
{
	[obj autorelease];
}

NSAutoreleasePool *newAutoreleasePool()
{
	return [[NSAutoreleasePool alloc] init];
}

unsigned int retainCount(id obj) {
    return [obj retainCount];
}

void releaseObjectWithPool(id obj)
{
    NSAutoreleasePool *pool = newAutoreleasePool();
    releaseObject(obj);
    releaseObject(pool);
}

void deallocObjectWithPool(id obj)
{
    NSAutoreleasePool *pool = newAutoreleasePool();
    deallocObject(obj);
    releaseObject(pool);
}

static SEL selRetain = 0;
static SEL selRelease = 0;

void retainSuper(id obj, Class cls)
{
    if(!selRetain)
        selRetain = sel_registerName("retain");
    
#if DO_LOG
    printf("retain super %p, %p\n",obj,cls);
#endif

    struct objc_super super;
    
#ifdef GNUSTEP
    super.self = obj;
    super.class = cls;
    
    objc_msg_lookup_super(&super, selRetain)(obj, selRetain);
#else
    super.receiver = obj;
    super.super_class = cls;
    
    objc_msgSendSuper(&super, selRetain);
#endif
}

void releaseSuper(id obj, Class cls)
{
    if(!selRelease)
        selRelease = sel_registerName("release");
    
#if DO_LOG
    printf("release super %p, %p\n",obj,cls);
#endif

    struct objc_super super;
    
#ifdef GNUSTEP
    super.self = obj;
    super.class = cls;
    
    objc_msg_lookup_super(&super, selRelease)(obj, selRelease);
#else
    super.receiver = obj;
    super.super_class = cls;
    
    objc_msgSendSuper(&super, selRelease);
#endif
}
