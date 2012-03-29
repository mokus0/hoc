#ifndef __Common_h__
#define __Common_h__

// Common includes for all cbits.
// takes care of importing the right ObjC runtime headers.

#ifdef GNUSTEP
#include <GNUstepBase/GSObjCRuntime.h>
#else
#include <objc/objc.h>
#include <objc/objc-runtime.h>
#endif

#include "Log.h"

#endif /* __Common_h__ */
