#include <Foundation/NSAutoreleasePool.h>

#include "Common.h"
#include "MemoryManagement.h"

NSAutoreleasePool *newAutoreleasePool()
{
	return [[NSAutoreleasePool alloc] init];
}
