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
import Extra

spec :: Spec
spec = describe "happiness" $ do
  it "compute correct happiness for submission-p22-2023-07-08T02_25_50.863957478Z.json" $ do
    Just prob <- readProblem 22
    Just (ans :: Answer) <- decode <$> BL.readFile "solutions/submission-p22-2023-07-08T02_25_50.863957478Z.json"
    h <- happiness prob ans
    h `shouldBe` 23370633
  it "compute correct happiness for arraying_026.json" $ do
    Just prob <- readProblem 26
    Just (ans :: Answer) <- decode <$> BL.readFile "solutions/arraying_026.json"
    h <- happiness prob ans
    h `shouldBe` 55122290
  it "compute correct happiness for submission-p22-2023-07-08T02_25_50.863957478Z.json" $ do
    Just prob <- readProblem 22
    Just (ans :: Answer) <- decode <$> BL.readFile "solutions/submission-p22-2023-07-08T02_25_50.863957478Z.json"
    extra <- mkExtra prob ans
    h <- withQueue extra prob ans
    h `shouldBe` 23370633
  it "compute correct happiness for arraying_026.json" $ do
    Just prob <- readProblem 26
    Just (ans :: Answer) <- decode <$> BL.readFile "solutions/arraying_026.json"
    extra <- mkExtra prob ans
    h <- withQueue extra prob ans
    h `shouldBe` 55122290
