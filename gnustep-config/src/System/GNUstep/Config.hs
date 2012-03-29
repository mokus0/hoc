{-# LANGUAGE DeriveDataTypeable #-}
module System.GNUstep.Config
    ( gnustepConfig
    , variable
    , objcFlags
    , objcLibs
    ) where

import Control.Applicative
import Data.Data (Data)
import Data.Typeable (Typeable)
import System.Directory
import System.FilePath
import System.Process

-- run a command and capture its output, dropping the terminal LF
backquote :: String -> [String] -> IO String
backquote cmd args = trim <$> readProcess cmd args ""

-- drop terminal LF if there is one
trim :: String -> String
trim ""     = ""
trim "\n"   = ""
trim (c:cs) = c : trim cs

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

gnustepConfig :: [String] -> IO String
gnustepConfig args = do
    bin <- findGNUstepConfig
    backquote bin args

variable :: String -> IO String
variable var = gnustepConfig ["--variable=" ++ var]

objcFlags :: IO String
objcFlags = gnustepConfig ["--objc-flags"]

objcLibs :: IO String
objcLibs = gnustepConfig ["--objc-libs"]

baseLibs :: IO String
baseLibs = gnustepConfig ["--base-libs"]

guiLibs :: IO String
guiLibs = gnustepConfig ["--gui-libs"]

data InstallationDomain
    = SYSTEM
    | NETWORK
    | LOCAL
    | USER
    deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable)

installationDomainFor :: String -> IO InstallationDomain
installationDomainFor pkg =
    read <$> gnustepConfig ["--installation-domain-for=" ++ pkg]