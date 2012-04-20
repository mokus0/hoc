#include "Common.h"
#include "Marshalling.h"
#include "HsFFI.h"

#define hsExceptionClassName "HOCHaskellException"
#define hsExceptionIvarName "_haskellException"

static BOOL excWrapperInited = NO;
static int stablePtrOffset;
static id clsHOCHaskellException;
static SEL selExceptionWithNameReasonUserInfo = 0;
static SEL selDealloc;
//static void initExceptionWrapper() __attribute__((constructor));

static void exc_dealloc(id self, SEL sel)
{
    hs_free_stable_ptr(* (HsStablePtr*) (((char*)self) + stablePtrOffset));
    struct objc_super super = {self, class_getSuperclass(object_getClass(self))};
    
#if GNUSTEP
    objc_msg_lookup_super(&super, selDealloc)(self, selDealloc);
#else
    objc_msgSendSuper(&super, selDealloc);
#endif
}

static void initExceptionWrapper()
{
    if(!excWrapperInited)
    {
        struct objc_ivar *stablePtrIvar;
        
        selDealloc = sel_registerName("dealloc");
        
        clsHOCHaskellException = objc_allocateClassPair(
                objc_getClass("NSException"),
                "HOCHaskellException", 0);
        
        class_addMethod(clsHOCHaskellException, selDealloc, (IMP) &exc_dealloc, "v@:");
        class_addIvar(clsHOCHaskellException, hsExceptionIvarName, sizeof(void *), 8, "^v");
            // the alignment parameter here is wrong... will be fixed when this code is
            // replaced by the version in the eobjc-ffi package.
        
        objc_registerClassPair(clsHOCHaskellException);
        
        stablePtrIvar = class_getInstanceVariable(clsHOCHaskellException, hsExceptionIvarName);
        stablePtrOffset = ivar_getOffset(stablePtrIvar);
        
        selExceptionWithNameReasonUserInfo = sel_registerName("exceptionWithName:reason:userInfo:");
        
        excWrapperInited = YES;
    }
}

id wrapHaskellException(char *name, HsStablePtr hexc)
{
    initExceptionWrapper();
    
    id cexc = objc_msgSend(clsHOCHaskellException, selExceptionWithNameReasonUserInfo,
                           utf8ToNSString("HaskellException"), utf8ToNSString(name), nil);
    
    // TODO: use proper function to get ivar
    * (HsStablePtr*) (((char*)cexc) + stablePtrOffset) = hexc;
    
    return cexc;
}

HsStablePtr unwrapHaskellException(id cexc)
{
    if(object_getClass(cexc) == clsHOCHaskellException)
    {
        return *(HsStablePtr*) (((char*)cexc) + stablePtrOffset);
    }
    else
        return nil;
}
