{-# OPTIONS_GHC -fwarn-unused-binds -fwarn-unused-imports #-}
module Main (main) where
import Control.Applicative
import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Simple.Setup
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import System.Exit( ExitCode(..) )
import System.FilePath
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

-- run a command and capture its output, dropping the terminal LF
backquote :: String -> [String] -> IO String
backquote cmd args = trim <$> readProcess cmd args ""

-- drop terminal LF if there is one
trim :: String -> String
trim ""     = ""
trim "\n"   = ""
trim (c:cs) = c : trim cs

getGccLibDir :: IO String
getGccLibDir = takeDirectory <$> backquote "gcc" ["--print-libgcc-file-name"]

getGNUstepVar :: String -> IO String
getGNUstepVar var = backquote "gnustep-config" ["--variable=" ++ var]

customConfig :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
customConfig pdbi cf = do
    cf <- setObjC2Flag cf
    
    lbi <- configure pdbi cf
    if System.Info.os == "darwin"
        then return()
        else do
            gcclibdir       <- getGccLibDir
            system_headers  <- getGNUstepVar "GNUSTEP_SYSTEM_HEADERS"
            system_libs     <- getGNUstepVar "GNUSTEP_SYSTEM_LIBRARIES"
            writeFile "HOC.buildinfo" $ unlines [
                "extra-lib-dirs: " ++ gcclibdir ++ ", " ++ system_libs,
                "include-dirs: " ++ system_headers ]

    return lbi
