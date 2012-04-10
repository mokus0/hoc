module Main where

import qualified TestFoundation

import Test.HUnit

import HOC.CBits( withAutoreleasePool )

main = withAutoreleasePool $ runTestTT $ TestFoundation.tests
