module Data.Draft where

import Data.Pack

data Draft = Draft
  { draftId :: Int
  } deriving (Eq, Show)

data DraftSnapshot = DraftSnapshot
  { draftMap :: Map User [Pack]
  }

data DraftEvent = DraftEvent
  { draftEventAction :: DraftAction
  , draftEventDraftId :: Int
  }

data DraftAction =
  DraftMove User
            Pack

data DraftUserCommand =
  DraftPick User
            Pack
            Card
            Card

draftSeed :: DraftSnapshot
draftSeed = DraftSnapshot empty False

processDraftUserCommand :: DraftSnapshot -> DraftUserCommand -> Maybe DraftAction
processDraftUserCommand (DraftSnapshot positions) (DraftMove user pack) = 

-- this goes somewhere else
handler :: _
handler draft (DraftPickCommand user pack c1 c2) = do
  pick <- processPackUserCommand pack (Pick c1 c2)
  add <- processPoolUserCommand user (Add c1 c2)
  draft <- processDraftUserCommand draft (DraftMove pack user)
  persist pick
  persist add
  persist draft
handler draft DraftStart = do
  draft <- processDraftUserCommand draft DraftStart
  persist draft
