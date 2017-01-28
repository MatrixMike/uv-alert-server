-- FIXME: Why doesn't hspec-discover work?
import Test.Hspec

import qualified TestFetcher
import qualified TestFetcherArpansa
import qualified TestFetcherJMA
import qualified TestTypes
import qualified TestTypesLocation
import qualified TestUtils
import qualified Integration.TestServer

main :: IO ()
main = hspec spec

spec = do
    describe "TestFetcher" TestFetcher.spec
    describe "TestFetcherArpansa" TestFetcherArpansa.spec
    describe "TestFetcherJMA" TestFetcherJMA.spec
    describe "TestTypes" TestTypes.spec
    describe "TestTypesLocation" TestTypesLocation.spec
    describe "TestUtils" TestUtils.spec
    describe "Integration.TestServer" Integration.TestServer.spec
