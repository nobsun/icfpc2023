{-# LANGUAGE OverloadedLists #-}
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
  -- 仕様書に載っている期待値の確認
  it "compute correct happiness for example problem" $ do
    Just prob <- decode <$> BL.readFile "example/problem.json"
    Just ans <- decode <$> BL.readFile "example/solution.json"
    h <- happiness prob ans
    h `shouldBe` 5343
  it "compute correct happiness for example problem with the 'Playing Together' extension" $ do
    Just prob <- decode <$> BL.readFile "example/problem.json"
    Just ans <- decode <$> BL.readFile "example/solution.json"
    -- Full division の問題として扱わせるためにダミーの pillar を追加している
    h <- happiness prob{ pillars = [ Pillar{ center = (1900,1900), radius = 1 } ] } ans
    h `shouldBe` 5357

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
  it "compute correct happiness for submission-p22 parallel" $ do
    Just prob <- readProblem 22
    Just (ans :: Answer) <- decode <$> BL.readFile "solutions/submission-p22-2023-07-08T02_25_50.863957478Z.json"
    extra <- mkExtra prob ans
    h <- withQueue extra prob ans
    h `shouldBe` 23370633
  it "compute correct happiness for arraying_026 parallel" $ do
    Just prob <- readProblem 26
    Just (ans :: Answer) <- decode <$> BL.readFile "solutions/arraying_026.json"
    extra <- mkExtra prob ans
    h <- withQueue extra prob ans
    h `shouldBe` 55122290
