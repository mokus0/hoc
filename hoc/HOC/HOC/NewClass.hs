module HOC.NewClass(
        HsIMP,
        MethodList,
        IvarList,
        newClass,
        makeMethodList,
        makeIvarList,
        setIvarInList,
        setMethodInList,
        makeDefaultIvarList,
        setHaskellRetainMethod,
        setHaskellReleaseMethod,
        setHaskellDataMethod
    ) where

import HOC.Base
import HOC.CBits
import HOC.ID
import HOC.Arguments

import Foreign.C.String
import Foreign.C.Types
import Foreign
import Foreign.LibFFI.Experimental
import Foreign.ObjC

newClass :: Ptr ObjCObject -> CString
             -> IvarList
             -> MethodList -> MethodList
             -> IO ()
newClass sc name (IvarList ivars) (MethodList ms) (MethodList cms) = 
    withForeignPtr ivars $ \ivars -> 
        withForeignPtr ms $ \ms ->
            withForeignPtr cms $ \cms -> do
                rawNewClass sc name ivars ms cms

makeIvarList :: Int -> IO IvarList
makeIvarList n = do
    ivars <- rawMakeIvarList n
    ivars <- newForeignPtr freePtr ivars
    return (IvarList ivars)

setIvarInList:: IvarList -> Int
                  -> CString -> CString -> CSize -> Word8 -> IO ()
setIvarInList (IvarList ivars) n name ty sz align = 
    withForeignPtr ivars $ \ivars -> do
        rawSetIvarInList ivars n name ty sz align

makeMethodList :: Int -> IO MethodList
makeMethodList n = do
    methods <- rawMakeMethodList n
    methods <- newForeignPtr freePtr methods
    return (MethodList methods)

setMethodInList :: MethodList -> Int -> SEL -> String -> SomeCIF -> HsIMP -> IO ()
setMethodInList (MethodList methodList) idx sel typ cif imp = 
    withForeignPtr methodList $ \methodList -> do
        typC <- newCString typ
        thunk <- wrapHsIMP imp
        rawSetMethodInList methodList idx sel typC cif thunk

makeDefaultIvarList :: IO IvarList
makeDefaultIvarList = do
    list <- makeIvarList 1
    name <- newCString "__retained_haskell_part__"
    typ <- newCString "^v"
    setIvarInList list 0 name typ 
        (fromIntegral $ sizeOf nullPtr)
        (fromIntegral $ alignment nullPtr)
    return list

retainSelector = getSelectorForName "retain"
retainCif = getCifForSelector (undefined :: ID () -> IO (ID ()))

releaseSelector = getSelectorForName "release"
releaseCif = getCifForSelector (undefined :: ID () -> IO ())

getHaskellDataSelector = getSelectorForName "__getHaskellData__"
getHaskellDataCif = getCifForSelector (undefined :: ID () -> IO (ID ()))
                                                -- actually  -> IO (Ptr ()) ...

setHaskellRetainMethod methodList idx super = 
    setMethodInList methodList idx retainSelector "@@:" retainCif (haskellObject_retain_IMP super)
    
setHaskellReleaseMethod methodList idx super = 
    setMethodInList methodList idx releaseSelector "v@:" releaseCif (haskellObject_release_IMP super)

setHaskellDataMethod methodList idx super mbDat = 
    setMethodInList methodList idx getHaskellDataSelector "^v@:" getHaskellDataCif (getHaskellData_IMP super mbDat)
