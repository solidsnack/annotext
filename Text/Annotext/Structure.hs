
module Text.Annotext.Structure where

import Data.ByteString

data Structure where
  URL :: ByteString -> Structure
  EMail :: ByteString -> Structure
  Plain :: ByteString -> Structure
  Bold :: ByteString -> Structure
  Smiley :: ByteString -> Structure

