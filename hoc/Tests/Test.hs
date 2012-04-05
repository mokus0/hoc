module Main where

import qualified TestPreprocessor
import qualified TestFoundation

import Test.HUnit

import HOC.CBits( withAutoreleasePool )

main = withAutoreleasePool $ runTestTT $ test [
        TestPreprocessor.tests,
        TestFoundation.tests
    ]
