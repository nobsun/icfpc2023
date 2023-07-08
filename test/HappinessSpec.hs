module HappinessSpec
  ( spec
  ) where

import qualified Data.ByteString.Lazy as BL
import Data.Aeson (decode)

import Test.Main
import Test.Hspec

import Answer
import Happiness
import Problem

spec :: Spec
spec = describe "happiness" $ do
  it "compute correct happiness for submission-p22-2023-07-08T02_25_50.863957478Z.json" $ do
    Just prob <- readProblem 22
    Just (ans :: Answer) <- decode <$> BL.readFile "solutions/submission-p22-2023-07-08T02_25_50.863957478Z.json"
    happiness prob ans `shouldBe` 23370633
