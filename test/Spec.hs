-- FIXME: Why doesn't hspec-discover work?
import Test.Hspec

import qualified TestFetcher
import qualified TestFetcherArpansa
import qualified Integration.TestLocations

main :: IO ()
main = hspec spec

spec = do
    describe "TestFetcher" TestFetcher.spec
    describe "TestFetcherArpansa" TestFetcherArpansa.spec
    describe "Integration.TestLocations" Integration.TestLocations.spec
