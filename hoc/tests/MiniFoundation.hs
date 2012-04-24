{-# LANGUAGE TemplateHaskell, StandaloneDeriving, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances, RankNTypes #-}
module MiniFoundation where

import HOC
import Foreign.ObjC (ObjCException(..))

import Foreign.C.Types
import Control.Exception
import System.IO.Unsafe     ( unsafePerformIO )
import Prelude hiding ( catch )

$(declareClass "NSObject" "ID")
$(declareClass "NSString" "NSObject")
$(declareClass "NSException" "NSObject")
$(declareClass "NSValue" "NSObject")
$(declareClass "NSNumber" "NSValue")
$(declareClass "NSArray" "NSObject")
$(declareClass "NSMutableArray" "NSArray")
$(declareClass "NSDictionary" "NSObject")

-- TODO: introduce CGFloat type?
$(declareCStructWithTag "NSPoint" (Prelude.Just "_NSPoint") [[t| Double |],[t| Double |]])
$(declareCStructWithTag "NSSize" (Prelude.Just "_NSSize") [[t| Double |],[t| Double |]])
$(declareCStructWithTag "NSRect" (Prelude.Just "_NSRect") [[t| NSPoint |],[t| NSSize |]])
$(declareCStructWithTag "NSRange" (Prelude.Just "_NSRange") [[t| CULong |],[t| CULong |]])

-- NSObject selectors
$(declareSelector "description" [t| IO (NSString ()) |])
$(declareSelector "init" [t| IO Inited |])
$(declareSelector "alloc" [t| IO Allocated |])

instance Has_init (NSObject a)
instance Has_description (NSObject a)
instance Has_alloc (NSObjectClass a)

-- NSString selectors
$(declareRenamedSelector "length" "nslength" [t| IO CUInt |])
$(declareSelector "initWithContentsOfFile:" [t| forall a . NSString a -> IO Inited |])
$(declareSelector "rangeOfString:" [t| NSString () -> IO NSRange |])

instance Has_nslength (NSString a)
instance Has_initWithContentsOfFile (NSString a)
instance Has_rangeOfString (NSString a)

-- NSValue selectors

$(declareSelector "initWithChar:" [t| CChar -> IO Inited |])
$(declareSelector "initWithShort:" [t| CShort -> IO Inited |])
$(declareSelector "initWithInt:" [t| CInt -> IO Inited |])
$(declareSelector "initWithLongLong:" [t| CLLong -> IO Inited |])
$(declareSelector "initWithFloat:" [t| Float -> IO Inited |])
$(declareSelector "initWithDouble:" [t| Double -> IO Inited |])
$(declareSelector "initWithBool:" [t| Bool -> IO Inited |])

$(declareSelector "numberWithInt:" [t| CInt -> IO (NSNumber ()) |])

$(declareSelector "charValue" [t| IO CChar |])
$(declareSelector "shortValue" [t| IO CShort |])
$(declareSelector "longLongValue" [t| IO CLLong |])
$(declareSelector "boolValue" [t| IO Bool |])
$(declareSelector "floatValue" [t| IO Float |])
$(declareSelector "doubleValue" [t| IO Double |])
$(declareSelector "intValue" [t| IO CInt |])

$(declareSelector "valueWithPoint:" [t| NSPoint -> IO (NSValue ()) |])
$(declareSelector "valueWithSize:" [t| NSSize -> IO (NSValue ()) |])
$(declareSelector "valueWithRect:" [t| NSRect -> IO (NSValue ()) |])
$(declareSelector "pointValue" [t| IO NSPoint |])
$(declareSelector "sizeValue" [t| IO NSSize |])
$(declareSelector "rectValue" [t| IO NSRect |])

instance Has_initWithChar (NSNumber a)
instance Has_initWithShort (NSNumber a)
instance Has_initWithInt (NSNumber a)
instance Has_initWithLongLong (NSNumber a)
instance Has_initWithFloat (NSNumber a)
instance Has_initWithDouble (NSNumber a)
instance Has_initWithBool (NSNumber a)

instance Has_numberWithInt (NSNumberClass a)

instance Has_charValue (NSNumber a)
instance Has_shortValue (NSNumber a)
instance Has_intValue (NSNumber a)
instance Has_longLongValue (NSNumber a)
instance Has_floatValue (NSNumber a)
instance Has_doubleValue (NSNumber a)
instance Has_boolValue (NSNumber a)

instance Has_valueWithPoint (NSValueClass a)
instance Has_valueWithSize (NSValueClass a)
instance Has_valueWithRect (NSValueClass a)
instance Has_pointValue (NSValue a)
instance Has_sizeValue (NSValue a)
instance Has_rectValue (NSValue a)

$(declareExternFun "NSStringFromSize" [t| NSSize -> IO (NSString ()) |])

-- NSException

$(declareRenamedSelector "exceptionWithName:reason:userInfo:"
                         "exceptionWithNameReasonUserInfo"
    [t| forall t1 t2 t3 . NSString t1 -> NSString t2 -> NSDictionary t3 -> IO (NSException ()) |])
$(declareSelector "name" [t| IO (NSString ()) |])
$(declareSelector "raise" [t| IO () |])

instance Has_exceptionWithNameReasonUserInfo (NSExceptionClass a)
instance Has_name (NSException a)
instance Has_raise (NSException a)

$(declareExternConst "NSParseErrorException" [t| NSString () |])


catchNS :: IO a -> (NSException () -> IO a) -> IO a

catchNS action handler
    = action `catch` \(ObjCException exc) -> importArgument exc >>= handler

-- NSMutableArray

$(declareSelector "addObject:" [t| forall t1 . ID t1 -> IO () |])
$(declareSelector "objectAtIndex:" [t| forall a. CUInt -> IO (ID a) |] )

instance Has_addObject (NSMutableArray a)
instance Has_objectAtIndex (NSMutableArray a)

deriving instance Show NSRect
deriving instance Show NSPoint
deriving instance Show NSSize
deriving instance Show NSRange

haskellString :: NSString a -> IO String
nsString :: String -> IO (NSString ())
haskellString nsstr = withExportedArgument nsstr importArgument
nsString nsstr = withExportedArgument nsstr importArgument
toNSString :: String -> NSString ()
toNSString = unsafePerformIO . nsString
fromNSString :: NSString () -> String
fromNSString = unsafePerformIO . haskellString
