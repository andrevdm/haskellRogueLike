{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aeson.Text.Extended
  ( encodeText
  , module Data.Aeson.Text
  )
  where

import Protolude
import qualified Data.Text.Lazy as TxtL
import qualified Data.Aeson as Ae
import           Data.Aeson.Text

encodeText :: Ae.ToJSON a => a -> Text
encodeText =
  TxtL.toStrict . encodeToLazyText
