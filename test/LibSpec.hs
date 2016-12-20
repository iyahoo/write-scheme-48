module LibSpec (spec) where

import Test.Hspec
import Lib

spec :: Spec
spec = do
  describe "double" $ do
    it "x2" $ do
      doubleX 3 `shouldBe` 6
