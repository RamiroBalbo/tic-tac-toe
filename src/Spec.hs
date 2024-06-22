module Spec where

import Game
import Prelude
import Test.Hspec
import Graphics.Gloss

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "El pdepreludat se instaló correctamente" $ do
      1+1 `shouldBe` 2

