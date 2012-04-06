#include <Foundation/Foundation.h>
#include <stdlib.h>

#include "Log.h"
#include "ObjectMap.h"

NSMapTable *gObjectMap = NULL;
int immortalCount = 0;

NSMapTable *getTheObjectMap()
{
    if(gObjectMap == NULL)
    {
        NSMapTableKeyCallBacks keyCallbacks;
        NSMapTableValueCallBacks valueCallbacks;
        
        memset(&keyCallbacks, 0, sizeof(keyCallbacks));
        keyCallbacks.notAKeyMarker = NSNotAPointerMapKey;
        
        memset(&valueCallbacks, 0, sizeof(valueCallbacks));

        gObjectMap = NSCreateMapTable(keyCallbacks, valueCallbacks, 0);
    }
    return gObjectMap;
}

/* 
 * This gets the HSO, given a obj-c part of an object.
 */
HsStablePtr getHaskellPart(id objcPart)
{
    void *hsPart = NSMapGet(getTheObjectMap(), objcPart);
#if DO_LOG
    NSLog(@"lookup %p -> %p", objcPart, hsPart);
#endif
    return hsPart;
}

void setHaskellPart(id objcPart, HsStablePtr haskellPart, HsBool immortal)
{   // assume that gObjectMap already exists
#if DO_LOG
    NSLog(@"new %p -> %d (immortal: %d)", objcPart, haskellPart, immortal);
#endif
    if(immortal)
    {
        if(!NSMapGet(gObjectMap, objcPart))
            immortalCount++;
#if DO_LOG
        else
            NSLog(@"### setHaskellPart ### -> immortal re-set");
#endif
    }
    return NSMapInsert(gObjectMap, objcPart, haskellPart);
}

void removeHaskellPart(id objcPart, HsStablePtr haskellPart)
{   // assume that gObjectMap already exists
    // don't remove if we no longer have the expected key
    // (finalizer ran to late and object was re-imported
    // to Haskell in the meantime)
    if(NSMapGet(gObjectMap, objcPart) == haskellPart)
    {
        NSMapRemove(gObjectMap, objcPart); 
#if DO_LOG
        NSLog(@"removed %p -> %d", objcPart, haskellPart);
#endif
    }
#if DO_LOG
    else
        NSLog(@"already reimported %p -> %d", objcPart, haskellPart);
#endif
}

void objectMapStatistics(unsigned *allocated, unsigned *immortal)
{
    *allocated = NSCountMapTable(getTheObjectMap());
    *immortal = immortalCount;
#if DO_LOG
    NSLog(@"statistics queried: %d - %d = %d",
            *allocated, *immortal, *allocated - *immortal);
#endif
}

