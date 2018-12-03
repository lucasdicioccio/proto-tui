{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module ProtoTui.Search where

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Lens
import Control.Lens.TH (makeLenses)
import Lens.Labels (HasLens')
import Proto.Google.Protobuf.Descriptor
import Proto.Google.Protobuf.Descriptor_Fields

data Search = Search {
    _searchText :: !Text
  } deriving (Show, Eq, Ord)

makeLenses ''Search

match :: HasLens' a "name" Text => Search -> a -> Bool
match (Search "") _ = True
match (Search str) obj = or [
    Text.toLower str `Text.isInfixOf` (obj ^. name . to (Text.toLower))
  ]

