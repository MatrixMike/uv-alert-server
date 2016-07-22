-- FIXME: Why doesn't hspec-discover work?
import Test.Hspec

import qualified TestFetcher
import qualified TestFetcherArpansa
import qualified TestFetcherEPA
import qualified TestFetcherJMA
import qualified TestUtils
import qualified Integration.TestLocations

main :: IO ()
main = hspec spec

spec = do
    describe "TestFetcher" TestFetcher.spec
    describe "TestFetcherArpansa" TestFetcherArpansa.spec
    describe "TestFetcherEPA" TestFetcherEPA.spec
    describe "TestFetcherJMA" TestFetcherJMA.spec
    describe "TestUtils" TestUtils.spec
    describe "Integration.TestLocations" Integration.TestLocations.spec
