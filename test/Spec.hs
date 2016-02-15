{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import {-@ HTF_TESTS @-} TestFetcher
import {-@ HTF_TESTS @-} TestFetcherArpansa
import {-@ HTF_TESTS @-} Integration.TestLocations

main = htfMain htf_importedTests
