{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
module HOC.ID
    ( getHOCImportStats
    , importClass
    , getHaskellDataForID
    ) where

import Control.Concurrent.MVar  ( MVar, newMVar, modifyMVar_, readMVar )
import Foreign.ForeignPtr       ( withForeignPtr )
import Foreign.ObjC             ( ObjCClass, ObjCObject, retainObject, autoreleaseObject )
import Foreign.ObjC.HSObject    ( HSO(..), importObject )
import Foreign.Ptr              ( Ptr, castPtr,  nullPtr )
import HOC.Arguments            ( ObjCArgument(..) )
import HOC.CBits
import System.IO.Unsafe         ( unsafePerformIO )
import System.Mem.Weak          ( addFinalizer )

dPutStrLn = if {--} False --} True
    then putStrLn
    else const $ return ()

dPutWords = dPutStrLn . unwords

-- TODO: track these better... 
--  in particular, track live objects rather than just imports and finalizers.
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
    
    withExportedArgument (ID (HSO fp _)) action =
        withForeignPtr fp action
    withExportedArgument Nil action = action (nullPtr)
    
    -- TODO: think more about whether this function can be eliminated...
    exportArgument (ID (HSO fp _)) =
        withForeignPtr fp $ \arg -> do
            retainObject arg
            autoreleaseObject arg
    exportArgument Nil = return nullPtr
   
    -- this time with no autorelease.  This method effectively claims
    -- ownership of the object.
    exportArgumentRetained (ID (HSO fp _)) =
        withForeignPtr fp retainObject
    exportArgumentRetained Nil = return nullPtr
    
    importArgument = importArgument' False

-- since we're using ForeignPtrs, do we even really care about "immortal"?
importClass :: Ptr ObjCClass -> IO (ID a)
importClass = importArgument' True . castPtr

importArgument' immortal p
    | p == nullPtr = return Nil
    | otherwise = do
        dPutWords ["importing", show p, if immortal then "(immortal)" else ""]
        hso@(HSO fp ds) <- importObject p
        dPutWords ["imported:", show fp, "(" ++ show (length ds), "dynamics)"]
        alterHOCImportStats $ \nAlloc nImm ->
            (nAlloc + 1, if immortal then nImm + 1 else nImm)
        if immortal
            then retainObject p >> return ()
            else addFinalizer hso $ do
                alterHOCImportStats $ \nAlloc nImm -> (nAlloc - 1, nImm)
                stats <- getHOCImportStats
                dPutWords ["finalized object", show p, "stats now", show stats]
        
        stats <- getHOCImportStats
        dPutStrLn ("did stats stuff... stats now " ++ show stats)
        return (ID hso)

getHaskellDataForID (ID (HSO _ dat)) = dat
getHaskellDataForID _                = []