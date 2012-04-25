{-# LANGUAGE TypeFamilies #-}
module HOC.ID
    ( ID, nil, castObject, idData
    , getImportedObjectCount
    , importClass
    ) where

import Control.Concurrent.MVar  ( MVar, newMVar, modifyMVar_, readMVar )
import Control.Monad            ( (>=>), when )
import Data.Dynamic             ( Dynamic )
import Foreign.ObjC             ( ObjCClass, ObjCObject, retainObject, autoreleaseObject )
import Foreign.ObjC.HSObject
import Foreign.Ptr              ( Ptr, castPtr,  nullPtr )
import HOC.Arguments            ( ObjCArgument(..) )
import System.IO.Unsafe         ( unsafePerformIO )

newtype ID a = ID HSO
    deriving (Eq)

instance Show (ID a) where
    showsPrec p (ID hso) = showsPrec p hso

nil :: ID a
nil = ID nilHSO

castObject :: ID a -> ID b
castObject (ID a) = ID a

idData :: ID a -> [Dynamic]
idData (ID hso) = hsoData hso

dPutStrLn = if {--} False --} True
    then putStrLn
    else const $ return ()

dPutWords = dPutStrLn . unwords

{-# NOINLINE hocImportedObjectCount #-}
hocImportedObjectCount :: MVar Int
hocImportedObjectCount = unsafePerformIO (newMVar 0)

alterImportedObjectCount :: (Int -> Int) -> IO ()
alterImportedObjectCount f = 
    modifyMVar_ hocImportedObjectCount (eval . f)
        where eval x = seq x (return x)

getImportedObjectCount :: IO Int
getImportedObjectCount = readMVar hocImportedObjectCount

instance ObjCArgument (ID a) where
    type ForeignArg (ID a) = Ptr ObjCObject
    
    withExportedArgument (ID hso) = withHSO hso
    
    exportArgument         (ID hso) = withHSO hso (retainObject >=> autoreleaseObject)
    exportArgumentRetained (ID hso) = withHSO hso  retainObject
    
    importArgument p
        | p == nullPtr = return nil
        | otherwise = do
            (hso, isNew) <- importObject p
            dPutWords ["imported", if isNew then "new" else "old", "object:"
                      , show hso, "(" ++ show (length (hsoData hso)), "dynamics)"]
            when isNew $ do
                alterImportedObjectCount succ
                addHSOFinalizer hso $ do
                    dPutWords ["released object", show hso]
                    alterImportedObjectCount pred
            
            return (ID hso)

importClass :: Ptr ObjCClass -> IO (ID a)
importClass = fmap ID . importObject_ . castPtr
