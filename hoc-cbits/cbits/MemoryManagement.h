#ifndef __MemoryManagement_h__
#define __MemoryManagement_h__

#include "Common.h"

@class NSAutoreleasePool;

void retainObject(id obj);
void releaseObject(id obj);

void retainSuper(id obj, Class cls);
void releaseSuper(id obj, Class cls);
unsigned int retainCount(id obj);

void deallocObject(id obj);

void autoreleaseObject(id obj);
NSAutoreleasePool *newAutoreleasePool();

void releaseObjectWithPool(id obj);
void deallocObjectWithPool(id obj);

#endif /* __MemoryManagement_h__ */
