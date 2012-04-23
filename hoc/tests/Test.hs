module Main where

import qualified TestFoundation

import Test.Framework (defaultMain, testGroup)

import HOC.CBits( withAutoreleasePool )

main = do
    TestFoundation.initializeHsClasses
    withAutoreleasePool $ defaultMain
        [ testGroup "TestFoundation" TestFoundation.tests
        ]
