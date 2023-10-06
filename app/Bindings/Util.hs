module Bindings.Util
  ( withText,
    withNull,
    cStringToText,
  )
where

import Data.ByteString (packCString, useAsCString)
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Foreign (Ptr, nullPtr)
import Foreign.C (CString)

withText :: Text -> (CString -> IO b) -> IO b
withText t = useAsCString (T.encodeUtf8 t)

withNull :: (Ptr a -> b) -> b
withNull f = f nullPtr

cStringToText :: CString -> IO Text
cStringToText cstr =
  T.decodeUtf8 <$> packCString cstr
