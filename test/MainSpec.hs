module MainSpec (main, spec) where

import Test.Hspec


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "" $ do
        it "" $ do
           0 `shouldBe` 0
