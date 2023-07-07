-- # 雛形モジュール
-- このファイルは`stack new`コマンドで自動的に`src/`に挿入されます
--
-- ## 言語拡張と`module`宣言
-- 最低限の指定をしてある
{- |
module:       Lib
copyright:    (c) Nobuo Yamashita 2022
license:      BSD-3
maintainer:   nobsun@sampou.org
stability:    experimental
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib
    ( someFunc
    ) where

import Data.ByteString.Lazy as B
import Data.Aeson
import Text.Printf (printf)
import GHC.Generics

{- |
print "some func" to stdout
>>> someFunc
some func
-}
someFunc :: IO ()
someFunc = putStrLn "some func"
