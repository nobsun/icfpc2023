{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
module Answer where

import Data.ByteString.Lazy as B
import Data.Aeson
import Text.Printf (printf)
import GHC.Generics

data Placement
  = Placement { x :: Float
              , y :: Float
              }
  deriving (Show, Eq, Generic)

data Answer
  = Answer { placements :: [Placement]
           }
  deriving (Show, Eq, Generic)

instance ToJSON Placement
instance FromJSON Placement
instance ToJSON Answer
instance FromJSON Answer


test :: B.ByteString
test = encode ans
  where ans = Answer { placements = [ Placement {x = 10.0, y = 20.0}
                                    , Placement {x = 30.0, y = 40.0}
                                    ]
                     }
