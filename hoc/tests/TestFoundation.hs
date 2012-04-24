{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances,
             MultiParamTypeClasses, RankNTypes, DeriveDataTypeable #-}
module TestFoundation where

import HOC

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
import Prelude hiding(init)

import Foreign.C.Types

import System.Mem           ( performGC )
import Control.Concurrent   ( threadDelay )
import Control.Monad        ( when )
import Control.Exception hiding ( assert )
import qualified System.Info( os )

import MiniFoundation

    -- (relative) path name of a text file that can be assumed to exist
testTextFile = "HOC.cabal"

      -- garbage collect and make really sure that finalizers have time to run
performGCAndWait targetCount time maxRepeat = do
    performGC
    threadDelay time
    actualCount <- getImportedObjectCount
    when (actualCount > targetCount && maxRepeat > 0) $
        performGCAndWait targetCount time (maxRepeat - 1)

assertLeaks leaks action = do
    initialCount <- getImportedObjectCount
    let targetCount = initialCount + leaks
    result <- action `finally` performGCAndWait targetCount 10000 25
    
    finalCount <- getImportedObjectCount
    assertEqual "Leaks" (finalCount - initialCount) leaks
    return result

assertNoLeaks action = assertLeaks 0 action


$(declareClass "HaskellObjectWithOutlet" "NSObject")

$(declareSelector "otherObject" [t| IO (ID ()) |])
$(declareSelector "setOtherObject:" [t| forall a. ID a -> IO () |])

instance Has_otherObject (HaskellObjectWithOutlet a)
instance Has_setOtherObject (HaskellObjectWithOutlet a)

$(declareSelector "maybeString" [t| IO (Maybe String) |])
$(declareSelector "setMaybeString:" [t| Maybe String -> IO () |] )

instance Has_maybeString (HaskellObjectWithOutlet a)
instance Has_setMaybeString (HaskellObjectWithOutlet a)

$(exportClass "HaskellObjectWithOutlet" "ho1_" [
        Outlet "otherObject" [t| ID () |],
        Outlet "maybeString" [t| NSString () |]
    ])

$(declareClass "HaskellObjectWithDescription" "NSObject")

$(exportClass "HaskellObjectWithDescription" "ho2_" [
        InstanceMethod 'description,
        InstanceMethod 'rectValue
    ])

instance Has_rectValue (HaskellObjectWithDescription a)

ho2_description self
    = do
        superDesc <- fmap fromNSString $ super self # description
        return $ toNSString $ head (words superDesc) ++ " TEST>"

ho2_rectValue self = return (NSRect (NSPoint 100 200) (NSSize 300 400))

$(declareClass "HaskellObjectWithIVar" "HaskellObjectWithOutlet")

$(exportClass "HaskellObjectWithIVar" "ho3_" [
        InstanceVariable "magicNumber" [t| Integer |] [| 0 |]
    ])


$(declareClass "ExceptionThrower" "NSObject")

$(declareSelector "throwHaskellException" [t| IO () |])
$(declareSelector "throwNSException" [t| IO () |])

instance Has_throwHaskellException (ExceptionThrower a)
instance Has_throwNSException (ExceptionThrower a)

$(exportClass "ExceptionThrower" "et_" [
        InstanceMethod 'throwHaskellException,
        InstanceMethod 'throwNSException
    ])

et_throwHaskellException self = fail "Test Exception"
et_throwNSException self = do
    -- Use nsString rather than toNSString; the latter will be
    -- let-floated to the top, screwing up our leak stats system
    fooBar <- nsString "FooBar"
    baz    <- nsString "baz"
    
    _NSException # exceptionWithNameReasonUserInfo
                 fooBar
                 baz
                 nil
     >>= raise

$(declareSelector "countInvocations:upto:" [t| Int -> Int -> IO Int |])

$(declareClass "HaskellObjectCountingInvocations" "NSObject")
$(exportClass "HaskellObjectCountingInvocations" "hoci_1_" [
        InstanceMethod 'countInvocationsUpto,
        ClassMethod 'countInvocationsUpto
    ])

instance Has_countInvocationsUpto (HaskellObjectCountingInvocations a)
instance Has_countInvocationsUpto (HaskellObjectCountingInvocationsClass a)

hoci_1_countInvocationsUpto start limit self = return (start + 1)

$(declareClass "HaskellObjectUsingSuper" "HaskellObjectCountingInvocations")
$(exportClass "HaskellObjectUsingSuper" "hoci_2_" [
        InstanceMethod 'countInvocationsUpto,
        ClassMethod 'countInvocationsUpto
    ])

hoci_2_countInvocationsUpto start limit self
    | start >= limit    = return start
    | otherwise         = super self # countInvocationsUpto (start + 1) limit

$(declareClass "HaskellSubclassOfObjectUsingSuper" "HaskellObjectUsingSuper")

$(exportClass "HaskellSubclassOfObjectUsingSuper" "noMembers_" [])

initializeHsClasses = do
    initializeClass_HaskellObjectWithOutlet
    initializeClass_HaskellObjectWithDescription
    initializeClass_HaskellObjectWithIVar
    initializeClass_ExceptionThrower
    initializeClass_HaskellObjectCountingInvocations
    initializeClass_HaskellObjectUsingSuper
    initializeClass_HaskellSubclassOfObjectUsingSuper

tests = [
        testGroup "NSNumber" [
            testCase "alloc-initWithInt-intValue" (assertNoLeaks $ do
                actual <- _NSNumber # alloc >>= initWithInt 42 >>= intValue
                actual @?= 42
            ),
            testCase "numberWithInt-intValue" (assertNoLeaks $ do
                actual <- _NSNumber # numberWithInt 42 >>= intValue
                actual @?= 42
            ),
            testCase "float" (assertNoLeaks $ do
                actual <- _NSNumber # alloc >>= initWithFloat 4.2 >>= floatValue
                actual @?= 4.2
            ),
            testCase "char" (assertNoLeaks $ do
                actual <- _NSNumber # alloc >>= initWithChar 42 >>= charValue
                actual @?= 42
            ),
            testCase "short" (assertNoLeaks $ do
                actual <- _NSNumber # alloc >>= initWithShort 42 >>= shortValue
                actual @?= 42
            ),
            testCase "char->int" (assertNoLeaks $ do
                actual <- _NSNumber # alloc >>= initWithChar 42 >>= intValue
                actual @?= 42
            ),
            testCase "int->char" (assertNoLeaks $ do
                actual <- _NSNumber # alloc >>= initWithInt 42 >>= charValue
                actual @?= 42
            ),
            testCase "bool" (assertNoLeaks $ do
                actual <- _NSNumber # alloc >>= initWithBool True >>= boolValue
                actual @?= True
                actual <- _NSNumber # alloc >>= initWithBool False >>= boolValue
                actual @?= False
            ),
            testCase "longlong" (assertNoLeaks $ do
                actual <- _NSNumber # alloc >>= initWithLongLong 0xDEADBEEFFACED00D
                                            >>= longLongValue
                actual @?= 0xDEADBEEFFACED00D
            )
        ],
        testGroup "NSString" [
            testCase "nsString-haskellString" (assertNoLeaks $ do
                actual <- nsString "Hello, world." >>= haskellString
                actual @?= "Hello, world."
            ),
            testCase "toNSString-fromNSString" (assertNoLeaks $ do
                fromNSString (toNSString "Hello, world.") @?= "Hello, world."
            ),
            testCase "initWithContentsOfFile" (assertNoLeaks $ do
                expected <- readFile testTextFile
                fileName <- nsString testTextFile
                actual_ns <- _NSString # alloc >>= initWithContentsOfFile fileName
                fromNSString actual_ns @?= expected
            ),
            testGroup "Unicode" $
                let zhongwen = "\x4E2D\x6587" -- "Chinese" in Chinese
                    fermata = "\x1D110"       -- Fermata (Musical Symbol)
                in [
                    testCase "nsString-haskellString" (assertNoLeaks $ do
                        actual <- nsString zhongwen >>= haskellString
                        actual @?= zhongwen
                    ),
                    testCase "length" (assertNoLeaks $ do
                        actual <- nsString zhongwen >>= nslength
                        actual @?= 2
                    ),
                    testCase "nsString-haskellString-fermata" (assertNoLeaks $ do
                        actual <- nsString fermata >>= haskellString
                        actual @?= fermata
                    ),
                    testCase "length-fermata" (assertNoLeaks $ do
                        actual <- nsString fermata >>= nslength
                        actual @?= 2 -- yes, 2. NSString uses UTF-16.
                    )
            ]
        ],
        testGroup "HaskellObjectWithOutlet" [
            testCase "alloc-init" (assertNoLeaks $ do
                _HaskellObjectWithOutlet # alloc >>= init >> return ()
            ),
            testCase "set-get" (assertNoLeaks $ do
                num <- _NSNumber # alloc >>= initWithInt 42
                hobj <- _HaskellObjectWithOutlet # alloc >>= init
                hobj # setOtherObject num
                num' <- hobj # otherObject >>= return . castObject
                assertEqual "Different Object returned." num num'
            ),
            testCase "set-forget-reget" (assertNoLeaks $ do
                -- set an ivar, 'forget' the object (stash it outside haskell-space),
                -- run the GC, 'remember' the object, and read the ivar.
                
                -- this catches a class of bug which helped me grok HSOs ;-)
                
                (num, array) <- assertLeaks 4 $ do
                    num <- _NSNumber # alloc >>= initWithInt 42         -- 1 import
                    hobj <- _HaskellObjectWithOutlet # alloc >>= init   -- 1 import
                    hobj # setOtherObject num   -- 1 import (arg in method body)
                    
                    array <- _NSMutableArray # alloc >>= init           -- 1 import
                    array # addObject hobj                              -- 0 imports
                    
                    return (num, array)
                    -- 0 releases: 1st 'num' is held by return, 2nd 'num' is held
                    -- by hobj, both 'hobj' refs are the same HSO, and 'array' is
                    -- held by return.
                
                num' <- assertLeaks (-2) $ do
                    hobj <- array # objectAtIndex 0 :: IO (HaskellObjectWithOutlet ())
                        -- 0 imports
                    
                    hobj # otherObject >>= return . castObject :: IO (NSNumber ())
                        -- 1 import (num' as return val)
                    -- array and hobj should now be released
                
                assertLeaks (-2) $ do
                    assertEqual "Different Objects returned." num num'
                    -- num and num' should now be released
            ),
            testCase "set-get-maybeString" (assertNoLeaks $ do
                hobj <- _HaskellObjectWithOutlet # alloc >>= init
                nothing <- hobj # maybeString
                nothing @?= Nothing
                
                hobj # setMaybeString (Just "42")
                just42 <- hobj # maybeString
                just42 @?= Just "42"
            )
        ],
        testGroup "HaskellObjectWithIVar" [
            testCase "alloc-init" (assertNoLeaks $ do
                _HaskellObjectWithIVar # alloc >>= init >> return ()
            ),
            testCase "set-get-superclass" (assertNoLeaks $ do
                num <- _NSNumber # alloc >>= initWithInt 42
                hobj <- _HaskellObjectWithIVar # alloc >>= init
                hobj # setOtherObject num
                num' <- hobj # otherObject >>= return . castObject
                assertEqual "Different Object returned." num num'
            ),
            testCase "set-get" (assertNoLeaks $ do
                hobj <- _HaskellObjectWithIVar # alloc >>= init
                hobj # setIVar _magicNumber 42
                answer <- hobj # getIVar _magicNumber
                when (answer /= 42) $ assert "Different Value returned."
            )
        ],
        testGroup "Memory" [
            testCase "NSMutableArray-Circle" (assertNoLeaks $ do
                array1 <- _NSMutableArray # alloc >>= init
                array2 <- _NSMutableArray # alloc >>= init
                array1 # addObject array2
                array2 # addObject array1
            ),
            testCase "NSMutableArray-Circle-with-Haskell" (assertLeaks 2 $ do
                hobj <- _HaskellObjectWithOutlet # alloc >>= init   -- 1 import
                array <- _NSMutableArray # alloc >>= init           -- 1 import
                array # addObject hobj                              -- 0 imports
                hobj # setOtherObject array                         -- 2 imports
                
                -- 1 release (the original 'array').
                -- 'hobj' (2 imports) is held by the array
                -- the array (its second import) is held by hobj.
            ),
            testCase "HaskellObjectCircle" (assertNoLeaks $ do
                hobj1 <- _HaskellObjectWithOutlet # alloc >>= init
                hobj2 <- _HaskellObjectWithOutlet # alloc >>= init
                hobj1 # setOtherObject hobj2
                hobj2 # setOtherObject hobj1
            )
        ],
        testGroup "nil" [
            testCase "eq" (assertNoLeaks $ do
                when (nil /= nil) $ assert "nil not equal"
            ),
            testCase "message" (assertNoLeaks $ do
                let nilNumber = nil :: NSNumber ()
                result <- try (nilNumber # intValue)
                expected <- try (fail "Message sent to nil: intValue")
                show (result :: Either SomeException CInt) 
                    @?= show (expected :: Either SomeException CInt)
            )            
        ],
        testGroup "Super" [
            testCase "description" (assertNoLeaks $ do
                hobj <- _HaskellObjectWithDescription # alloc >>= init
                str <- hobj # description
                fromNSString str @?= "<HaskellObjectWithDescription: TEST>"
            ),
            testGroup "instanceChaining" [
                testCase "base" (assertNoLeaks $ do
                    hobj <- _HaskellObjectCountingInvocations # alloc >>= init
                    count <- hobj # countInvocationsUpto 0 100
                    count @?= 1
                ),
                testCase "subclass" (assertNoLeaks $ do
                    hobj <- _HaskellObjectUsingSuper # alloc >>= init
                    count <- hobj # countInvocationsUpto 0 100
                    count @?= 2
                ),
                testCase "subsubclass" (assertNoLeaks $ do
                    hobj <- _HaskellSubclassOfObjectUsingSuper # alloc >>= init
                    count <- hobj # countInvocationsUpto 0 100
                    count @?= 2
                )
                
            ],
            testGroup "classChaining" [
                testCase "base" (assertNoLeaks $ do
                    count <- _HaskellObjectCountingInvocations # countInvocationsUpto 0 100
                    count @?= 1
                ),
                testCase "subclass" (assertNoLeaks $ do
                    count <- _HaskellObjectUsingSuper # countInvocationsUpto 0 100
                    count @?= 2
                ),
                testCase "subsubclass" (assertNoLeaks $ do
                    count <- _HaskellSubclassOfObjectUsingSuper # countInvocationsUpto 0 100
                    count @?= 2
                )
                
            ]
        ],
        testGroup "structs" [
            testCase "pointArg" (assertNoLeaks $ do
                let point = NSPoint 6.42 7.42
                result <- _NSValue # valueWithPoint point
                return ()
            ),
            testCase "point" (assertNoLeaks $ do
                let point = NSPoint 6.42 7.42
                result <- _NSValue # valueWithPoint point >>= pointValue
                result @?= point
            ),
            testCase "size" (assertNoLeaks $ do
                let size = NSSize 6.42 7.42
                result <- _NSValue # valueWithSize size >>= sizeValue
                result @?= size
            ),
            testCase "rect" (assertNoLeaks $ do
                let rect = NSRect (NSPoint 1 2) (NSSize 3 4)
                result <- _NSValue # valueWithRect rect >>= rectValue
                result @?= rect
            ),
            testCase "range" (assertNoLeaks $ do
                let range = NSRange 25 3
                homebrew <- nsString "Imperial India Pale Ale (IPA)"
                ipa      <- nsString "IPA"
                result <- homebrew # rangeOfString ipa
                result @?= range
            ),
            testCase "HaskellObjectStructRet" (assertNoLeaks $ do
                hobj <- _HaskellObjectWithDescription # alloc >>= init
                rect     <- hobj # rectValue
                expected <- hobj # ho2_rectValue
                rect @?= expected
            )
        ],
        testCase "externConstant" (assertNoLeaks $ 
            fromNSString nsParseErrorException @?= "NSParseErrorException"
        ),
        testCase "externFunction" (assertNoLeaks $ do
            result <- nsStringFromSize (NSSize 42 23)
            fromNSString result @?= if System.Info.os == "darwin" then "{42, 23}" else "{width = 42; height = 23}"
        ),
        testGroup "exceptions" [
            testCase "CtoH" (assertNoLeaks $ do
                -- Use nsString rather than toNSString; the latter will be
                -- let-floated to the top, screwing up our leak stats system
                fooBar <- nsString "FooBar"
                baz    <- nsString "baz"
                exc1 <- _NSException # exceptionWithNameReasonUserInfo
                                        fooBar
                                        baz
                                        nil
                result <- (exc1 # raise >> return "No Exception")
                        `catchNS` \e -> e # name >>= return . fromNSString 
                result @?= "FooBar"
            ),
            testCase "HtoCtoH" (assertNoLeaks $ do
                obj <- _ExceptionThrower # alloc >>= init
                result <- try (obj # throwHaskellException)
                show (result :: Either SomeException ()) 
                    @?= "Left user error (Test Exception)"
            ),
            testCase "CtoHtoCtoH" (assertNoLeaks $ do
                obj <- _ExceptionThrower # alloc >>= init
                result <- (obj # throwNSException >> return "No Exception")
                        `catchNS` \e -> e # name >>= return . fromNSString 
                result @?= "FooBar"
            )
        ]
    ]

