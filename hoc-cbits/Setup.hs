{-# OPTIONS_GHC -fwarn-unused-binds -fwarn-unused-imports #-}
module Main (main) where
import Control.Applicative
import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Simple.Setup
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import System.Directory
import System.FilePath
import System.Process
import qualified System.Info

main = defaultMainWithHooks $ defaultUserHooks {
        confHook = customConfig
    }

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

findGNUstepConfig :: IO String
findGNUstepConfig = findExecutable "gnustep-config" >>= search dirs
    where
        search _ (Just bin) = return bin
        search [] _         = fail "Can't find gnustep-config command"
        search (dir:dirs) _ = check dir >>= search dirs
        
        check dir = do
            let file = dir </> "gnustep-config"
            exists <- doesFileExist file
            return (if exists then Just file else Nothing)
        
        dirs = 
            [ base </> sub </> "GNUstep/System/Tools" 
            | base <- ["/usr", "/opt"]
            , sub  <- ["", "local"]
            ]

getGNUstepVar :: String -> IO String
getGNUstepVar var = do
    bin <- findGNUstepConfig
    backquote bin ["--variable=" ++ var]

customConfig :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
customConfig pdbi cf = do
    lbi <- configure pdbi cf
    if System.Info.os == "darwin"
        then return()
        else do
            gcclibdir       <- getGccLibDir
            system_headers  <- getGNUstepVar "GNUSTEP_SYSTEM_HEADERS"
            system_libs     <- getGNUstepVar "GNUSTEP_SYSTEM_LIBRARIES"
            writeFile "HOC-cbits.buildinfo" $ unlines [
                "extra-lib-dirs: " ++ gcclibdir ++ ", " ++ system_libs,
                "include-dirs: " ++ system_headers ]

    return lbi
