{-# LANGUAGE OverloadedStrings #-}
module LibSpec
  ( spec
  ) where

import Data.String
import qualified Data.ByteString as B
import Test.Main
import Test.Hspec
import Lib

spec :: Spec
spec = describe "someFunc" $ do
  { it "「なんか関数」を標準出力に印字する." $ do
    { result <- captureProcessResult Lib.someFunc
    ; prExitCode result `shouldBe` ExitSuccess
    ; prStderr result `shouldSatisfy` B.null
    ; prStdout result `shouldBe` "some func\n"
    }
  }
