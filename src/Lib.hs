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
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
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

type Instrument = Int
type Like = Float

data Attendee
  = Attendee { x :: Float
             , y :: Float
             , tastes :: [Like]
             }
  deriving (Show, Eq, Generic)

data Problem
  = Problem { room_width :: Float
            , room_height :: Float
            , stage_width :: Float
            , stage_height :: Float
            , musicians :: [Instrument]
            , attendees :: [Attendee]
            }
  deriving (Show, Eq, Generic)

instance ToJSON Attendee
instance FromJSON Attendee
instance ToJSON Problem
instance FromJSON Problem

test :: Int -> IO ()
test q = do
  inp <- B.readFile (printf "problems/%03d.json" q)
  let problem = (decode inp :: Maybe Problem)
  print problem
