module Main where

import qualified TestPreprocessor

import Test.HUnit

import HOC.CBits( withAutoreleasePool )

main = withAutoreleasePool $ runTestTT $ test TestPreprocessor.tests
