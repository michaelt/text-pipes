module Pipes.Text.Internal
    ( Decoding(..)
    , streamDecodeUtf8
    , decodeSomeUtf8
    , Codec(..)
    , TextException(..)
    , utf8
    , utf16_le
    , utf16_be
    , utf32_le
    , utf32_be
    ) where

import Pipes.Text.Internal.Decoding
import Pipes.Text.Internal.Codec