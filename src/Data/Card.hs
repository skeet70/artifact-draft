module Data.Card where

import Data.Text (Text)

-- TODO: expand to encompass the Artifact CardSet API response.
-- TODO: seed database with response, or setup something to poll it regularly?
data Card = Card
  { cardName :: Text
  } deriving (Eq, Show)
