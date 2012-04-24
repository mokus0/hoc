{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
module HOC.ID
    ( getHOCImportStats
    , importClass
    , getHaskellDataForID
    ) where

import Control.Concurrent.MVar  ( MVar, newMVar, modifyMVar_, readMVar, withMVar )
import Control.Monad            ( when )
import Foreign.ObjC             ( ObjCClass, ObjCObject, retainObject, autoreleaseObject )
import Foreign.ObjC.HSObject    ( withHSO, hsoData, addHSOFinalizer, importObject )
import Foreign.Ptr              ( Ptr, castPtr,  nullPtr )
import HOC.Arguments            ( ObjCArgument(..) )
import HOC.CBits
import System.IO                ( hFlush, stdout )
import System.IO.Unsafe         ( unsafePerformIO )

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
    
    withExportedArgument (ID hso) action = withHSO hso action
    withExportedArgument Nil action = action nullPtr
    
    -- TODO: think more about whether this function can be eliminated...
    exportArgument (ID hso) = withHSO hso $ \arg -> do
        retainObject arg
        autoreleaseObject arg
    exportArgument Nil = return nullPtr
   
    -- this time with no autorelease.  This method effectively claims
    -- ownership of the object.
    exportArgumentRetained (ID hso) =
        withHSO hso retainObject
    exportArgumentRetained Nil = return nullPtr
    
    importArgument = importArgument' False

-- since we're using ForeignPtrs, do we even really care about "immortal"?
importClass :: Ptr ObjCClass -> IO (ID a)
importClass = importArgument' True . castPtr

importArgument' immortal p
    | p == nullPtr = return Nil
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

getHaskellDataForID (ID hso)    = hsoData hso
getHaskellDataForID _           = []