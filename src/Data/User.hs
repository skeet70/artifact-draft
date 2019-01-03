module Data.User where

import Data.Text (Text)
import Eventful

data User = User
  { userName :: Text
  } deriving (Show, Eq)
