module FunctorSpec (spec) where

import MyFunctors
import Test.Hspec

spec :: Spec
spec = do
  describe "MyFunctors" $ do
    describe "MyFunctor Maybe" $ do
      it "can represent a container that has a value of type a or Nothing" $ do
        fmap' id Nothing `shouldBe` (Nothing :: Maybe Int)
        fmap' id (Just 1) `shouldBe` (Just 1 :: Maybe Int)

    describe "MyEither" $ do
      it "can represent a container with either a value of type a or a value of type e" $ do
        pending