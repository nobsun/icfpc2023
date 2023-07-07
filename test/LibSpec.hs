{-# LANGUAGE OverloadedStrings #-}
module LibSpec
  ( spec
  ) where

import qualified Data.ByteString as B
import Test.Main
import Test.Hspec
import Lib

spec :: Spec
spec = describe "someFunc" $ do
  { it "print somewhat from someFunc" $ do
    { result <- captureProcessResult Lib.someFunc
    ; prExitCode result `shouldBe` ExitSuccess
    ; prStderr result `shouldSatisfy` B.null
    ; prStdout result `shouldBe` "some func\n"
    }
  }
