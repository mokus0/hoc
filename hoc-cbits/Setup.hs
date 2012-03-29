{-# OPTIONS_GHC -fwarn-unused-binds -fwarn-unused-imports #-}
module Main (main) where
import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Simple.Setup
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import qualified System.GNUstep.Config as GNUstep
import qualified System.Info

main = defaultMainWithHooks $ defaultUserHooks {
        confHook = customConfig
    }

customConfig :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
customConfig pdbi cf = do
    lbi <- configure pdbi cf
    if System.Info.os == "darwin"
        then return()
        else do
            system_headers  <- GNUstep.variable "GNUSTEP_SYSTEM_HEADERS"
            system_libs     <- GNUstep.variable "GNUSTEP_SYSTEM_LIBRARIES"
            writeFile "HOC-cbits.buildinfo" $ unlines [
                "extra-lib-dirs: " ++ system_libs,
                "include-dirs: " ++ system_headers ]

    return lbi
