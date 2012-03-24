{-# OPTIONS_GHC -fwarn-unused-binds -fwarn-unused-imports #-}
module Main (main) where
import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Simple.Setup
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import System.Exit( ExitCode(..) )
import System.FilePath
import System.IO
import System.Process
import qualified System.Info

main = defaultMainWithHooks $ defaultUserHooks {
        confHook = customConfig
    }

objc2_flagName = FlagName "objc2"

setObjC2Flag :: ConfigFlags -> IO ConfigFlags
setObjC2Flag cf
    -- if the flag is set on the command line, do nothing
    | lookup (objc2_flagName) (configConfigurationsFlags cf) /= Nothing
    = return cf
    
    -- if we're not on darwin, assume false
    | System.Info.os /= "darwin"
    = return $ addFlag objc2_flagName False cf
    
    -- otherwise make an educated guess
    | otherwise
    = do
        value <- objC2Available
        return $ addFlag objc2_flagName value cf
    
    where addFlag flag value cf = cf { configConfigurationsFlags = 
                (flag,value) : configConfigurationsFlags cf }

objC2Available :: IO Bool
objC2Available 
    | System.Info.os /= "darwin"    = return False
    | otherwise = do
        result <- system "grep -qR /usr/include/objc -e objc_allocateClassPair"
        return (result == ExitSuccess)

backquote :: String -> IO String
backquote cmd = do
    (inp,out,err,pid) <- runInteractiveCommand cmd
    hClose inp
    text <- hGetContents out
    waitForProcess pid
    hClose err
    return $ init text ++ let c = last text in if c == '\n' then [] else [c]

gnustepPaths :: IO (String, String, String)
gnustepPaths = do
    libgcc <- backquote "gcc --print-libgcc-file-name"
    headersAndLibraries <- backquote
            "opentool /bin/sh -c \
            \'. $GNUSTEP_MAKEFILES/filesystem.sh \
            \; echo $GNUSTEP_SYSTEM_HEADERS ; echo $GNUSTEP_SYSTEM_LIBRARIES'"

    let gcclibdir =  takeDirectory libgcc

    let system_headers : system_libs : _ = lines headersAndLibraries    
    -- sysroot <- getEnv "GNUSTEP_SYSTEM_ROOT"
    -- let system_headers = gnustepsysroot </> "Library/Headers"
    --    system_libs = gnustepsysroot </> "Library/Libraries"
    return (gcclibdir, system_libs, system_headers)

customConfig :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
customConfig pdbi cf = do
    cf <- setObjC2Flag cf
    
    lbi <- configure pdbi cf
    if System.Info.os == "darwin"
        then return()
        else do
            (gcclibdir, system_libs, system_headers) <- gnustepPaths
            writeFile "HOC.buildinfo" $ unlines [
                "extra-lib-dirs: " ++ gcclibdir ++ ", " ++ system_libs,
                "include-dirs: " ++ system_headers ]

    return lbi