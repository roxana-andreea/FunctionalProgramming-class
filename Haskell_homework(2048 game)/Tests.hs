{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Tests where

import Test.Framework

import {-@ HTF_TESTS @-} BoardTest
import {-@ HTF_TESTS @-} AIEmptyTest
import {-@ HTF_TESTS @-} AIExpectimaxTest

main :: IO ()
main = htfMain htf_importedTests
