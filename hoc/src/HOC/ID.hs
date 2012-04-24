{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
module HOC.ID
    ( ID, nil, castObject, idData
    , getHOCImportStats
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

{-# NOINLINE hocImportStats #-}
hocImportStats :: MVar (Int, Int) {- allocated, immortal -}
hocImportStats = unsafePerformIO (newMVar (0,0))

alterHOCImportStats :: (Int -> Int -> (Int, Int)) -> IO ()
alterHOCImportStats f = 
    modifyMVar_ hocImportStats (evalPair . uncurry f)
        where evalPair (!a, !b) = return (a, b)

getHOCImportStats :: IO (Int, Int)
getHOCImportStats = readMVar hocImportStats

instance ObjCArgument (ID a) where
    type ForeignArg (ID a) = Ptr ObjCObject
    
    withExportedArgument (ID hso) = withHSO hso
    
    exportArgument         (ID hso) = withHSO hso (retainObject >=> autoreleaseObject)
    exportArgumentRetained (ID hso) = withHSO hso  retainObject
    
    importArgument = importArgument' False

-- since we're using ForeignPtrs, do we even really care about "immortal"?
importClass :: Ptr ObjCClass -> IO (ID a)
importClass = importArgument' True . castPtr

importArgument' immortal p
    | p == nullPtr = return nil
    | otherwise = do
        (hso, isNew) <- importObject p
        dPutWords ["imported", if isNew then "new" else "old"
                  , if immortal then "class:" else "object:", show hso
                  , "(" ++ show (length (hsoData hso)), "dynamics)"]
        when isNew $ do
            alterHOCImportStats $ \nAlloc nImm ->
                (nAlloc + 1, if immortal then nImm + 1 else nImm)
            if immortal
                then retainObject p >> return ()
                else addHSOFinalizer hso $ do
                    alterHOCImportStats $ \nAlloc nImm -> (nAlloc - 1, nImm)
        
        stats <- getHOCImportStats
        return (ID hso)
