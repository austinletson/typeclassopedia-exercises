module FunctorSpec (spec) where

import Functors
import Test.Hspec

spec :: Spec
spec = do
  describe "Functors" $ do
    describe "MyMaybe" $ do
      it "can represent a container that has a value of type a or MyNothing" $ do
        pending

    describe "MyEither" $ do
      it "can represent a container with either a value of type a or a value of type e" $ do
        pending