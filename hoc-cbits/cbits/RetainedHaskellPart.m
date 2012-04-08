#include <assert.h>
#include <stdlib.h>
#ifdef GNUSTEP
#include <objc/objc-api.h>
#else
#include <objc/objc-class.h>
#endif
#include "RetainedHaskellPart.h"
#include "Ivars.h"

HsStablePtr getRetainedHaskellPart(id obj)
{
    HsStablePtr haskellPart = NULL;
    struct objc_ivar *ivar = object_getInstanceVariable( obj,
                                            "__retained_haskell_part__",
                                            &haskellPart );
    assert(ivar);
    return haskellPart;
}

void setRetainedHaskellPart(id obj, HsStablePtr haskellPart)
{
    struct objc_ivar *ivar = object_setInstanceVariable( obj,
                                            "__retained_haskell_part__",
                                            haskellPart );
    assert(ivar);
}
