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

import Data.Word                    ( Word8 )
import Foreign.C.String             ( CString, newCString )
import Foreign.C.Types              ( CSize )
import Foreign.ForeignPtr           ( newForeignPtr, withForeignPtr )
import Foreign.LibFFI.Experimental  ( CIF, cif )
import Foreign.ObjC                 ( SEL, ObjCSigType, sigTypeString )
import Foreign.Ptr                  ( Ptr, nullPtr )
import Foreign.Storable             ( sizeOf, alignment )
import HOC.Arguments                ( ForeignSel )
import HOC.Base                     ( getSelectorForName )
import HOC.CBits
import HOC.ID
import HOC.StdArgumentTypes         ({- instances -})

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

setMethodInList :: ObjCSigType a => MethodList -> Int -> SEL -> CIF (Ptr ObjCObject -> SEL -> a) -> HsIMP a -> IO ()
setMethodInList (MethodList methodList) idx sel cif imp = 
    withForeignPtr methodList $ \methodList -> do
        typC <- newCString (sigTypeString cif)
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
retainCif = cif :: CIF (ForeignSel (ID () -> IO (ID ())))

releaseSelector = getSelectorForName "release"
releaseCif = cif :: CIF (ForeignSel (ID () -> IO ()))

getHaskellDataSelector = getSelectorForName "__getHaskellData__"
getHaskellDataCif = cif :: CIF (ForeignSel (ID () -> IO (Ptr ())))
                                                -- actually  -> IO (Ptr ()) ...

setHaskellRetainMethod methodList idx super = 
    setMethodInList methodList idx retainSelector retainCif (haskellObject_retain_IMP super)
    
setHaskellReleaseMethod methodList idx super = 
    setMethodInList methodList idx releaseSelector releaseCif (haskellObject_release_IMP super)

setHaskellDataMethod methodList idx super mbDat = 
    setMethodInList methodList idx getHaskellDataSelector getHaskellDataCif (getHaskellData_IMP super mbDat)
