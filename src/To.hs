{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module To
(
    ToText(..),
    ToLazyText(..),
    ToBuilder(..),
    ToString(..),
    ToByteString(..),
    ToLazyByteString(..),
    Utf8ToString(..),
    Utf8ToText(..),
    Utf8ToLazyText(..),
    Utf8ToBuilder(..),
)
where

import GHC.TypeLits (TypeError, ErrorMessage(..))
import Data.Text
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as UTF8L
import qualified Data.ByteString.UTF8 as UTF8

----------------------------------------------------------------------------
-- ToText
----------------------------------------------------------------------------

class ToText a where
    -- | Transforming to strict Text.
    toText :: a -> Text

instance (a ~ Char) => ToText [a] where
    toText = pack
    {-# INLINE toText #-}

instance ToText TL.Text where
    toText = TL.toStrict
    {-# INLINE toText #-}

instance ToText Builder where
    toText = TL.toStrict . B.toLazyText
    {-# INLINE toText #-}

instance TypeError
    ('Text "Can not decode a ByteString without specifying encoding." :$$:
     'Text "Use 'utf8ToText' if you want to decode ASCII or UTF8.")
    =>
    ToText BS.ByteString where
    toText = error "unreachable"

instance TypeError
    ('Text "Can not decode a lazy ByteString without specifying encoding." :$$:
     'Text "Use 'utf8ToText' if you want to decode ASCII or UTF8.")
    =>
    ToText BSL.ByteString where
    toText = error "unreachable"

----------------------------------------------------------------------------
-- ToLazyText
----------------------------------------------------------------------------

class ToLazyText a where
    -- | Transforming to lazy Text.
    toLazyText :: a -> TL.Text

instance (a ~ Char) => ToLazyText [a] where
    toLazyText = TL.pack
    {-# INLINE toLazyText #-}

instance ToLazyText Text where
    toLazyText = TL.fromStrict
    {-# INLINE toLazyText #-}

instance ToLazyText Builder where
    toLazyText = B.toLazyText
    {-# INLINE toLazyText #-}

instance TypeError
    ('Text "Can not decode a ByteString without specifying encoding." :$$:
     'Text "Use 'utf8ToLazyText' if you want to decode ASCII or UTF8.")
    =>
    ToLazyText BS.ByteString where
    toLazyText = error "unreachable"

instance TypeError
    ('Text "Can not decode a lazy ByteString without specifying encoding." :$$:
     'Text "Use 'utf8ToLazyText' if you want to decode ASCII or UTF8.")
    =>
    ToLazyText BSL.ByteString where
    toLazyText = error "unreachable"

----------------------------------------------------------------------------
-- ToBuilder
----------------------------------------------------------------------------

class ToBuilder a where
    -- | Transforming to Builder.
    toBuilder :: a -> Builder

instance (a ~ Char) => ToBuilder [a] where
    toBuilder = B.fromString
    {-# INLINE toBuilder #-}

instance ToBuilder Text where
    toBuilder = B.fromText
    {-# INLINE toBuilder #-}

instance ToBuilder TL.Text where
    toBuilder = B.fromLazyText
    {-# INLINE toBuilder #-}

instance TypeError
    ('Text "Can not decode a ByteString without specifying encoding." :$$:
     'Text "Use 'utf8ToBuilder' if you want to decode ASCII or UTF8.")
    =>
    ToBuilder BS.ByteString where
    toBuilder = error "unreachable"

instance TypeError
    ('Text "Can not decode a lazy ByteString without specifying encoding." :$$:
     'Text "Use 'utf8ToBuilder' if you want to decode ASCII or UTF8.")
    =>
    ToBuilder BSL.ByteString where
    toBuilder = error "unreachable"

----------------------------------------------------------------------------
-- ToString
----------------------------------------------------------------------------

class ToString a where
    -- | Transforming to String
    toString :: a -> String

instance ToString Text where
    toString = unpack
    {-# INLINE toString #-}

instance ToString TL.Text where
    toString = TL.unpack
    {-# INLINE toString #-}

instance ToString Builder where
    toString = TL.unpack . B.toLazyText
    {-# INLINE toString #-}

instance TypeError
    ('Text "Can not decode a ByteString without specifying encoding." :$$:
     'Text "Use 'utf8ToString' if you want to decode ASCII or UTF8.")
    =>
    ToString BS.ByteString where
    toString = error "unreachable"

instance TypeError
    ('Text "Can not decode a lazy ByteString without specifying encoding." :$$:
     'Text "Use 'utf8ToString' if you want to decode ASCII or UTF8.")
    =>
    ToString BSL.ByteString where
    toString = error "unreachable"

----------------------------------------------------------------------------
-- ToByteString
----------------------------------------------------------------------------

class ToByteString a where
    -- | Transforming to strict ByteString.
    toByteString :: a -> BS.ByteString

instance ToByteString Text where
    toByteString = encodeUtf8
    {-# INLINE toByteString #-}

instance ToByteString TL.Text where
    toByteString = encodeUtf8 . TL.toStrict
    {-# INLINE toByteString #-}

instance ToByteString Builder where
    toByteString = encodeUtf8 . TL.toStrict . B.toLazyText
    {-# INLINE toByteString #-}

instance (a ~ Char) => ToByteString [a] where
    toByteString = UTF8.fromString
    {-# INLINE toByteString #-}

instance ToByteString BSL.ByteString where
    toByteString = BSL.toStrict
    {-# INLINE toByteString #-}

----------------------------------------------------------------------------
-- ToLazyByteString
----------------------------------------------------------------------------

class ToLazyByteString a where
    -- | Transforming to lazy ByteString.
    toLazyByteString :: a -> BSL.ByteString

instance ToLazyByteString Text where
    toLazyByteString = TL.encodeUtf8 . TL.fromStrict
    {-# INLINE toLazyByteString #-}

instance ToLazyByteString TL.Text where
    toLazyByteString = TL.encodeUtf8
    {-# INLINE toLazyByteString #-}

instance ToLazyByteString Builder where
    toLazyByteString = TL.encodeUtf8 . B.toLazyText
    {-# INLINE toLazyByteString #-}

instance (a ~ Char) => ToLazyByteString [a] where
    toLazyByteString = UTF8L.fromString
    {-# INLINE toLazyByteString #-}

instance ToLazyByteString BS.ByteString where
    toLazyByteString = BSL.fromStrict
    {-# INLINE toLazyByteString #-}

----------------------------------------------------------------------------
-- Utf8ToString
----------------------------------------------------------------------------

class Utf8ToString a where
    -- | Decode UTF8-encoded text to a 'String'.
    --
    -- Malformed characters are replaced by @\\0xFFFD@ (the Unicode
    -- replacement character).
    utf8ToString :: a -> String

instance Utf8ToString BS.ByteString where
    utf8ToString = UTF8.toString
    {-# INLINE utf8ToString #-}

instance Utf8ToString BSL.ByteString where
    utf8ToString = UTF8L.toString
    {-# INLINE utf8ToString #-}

----------------------------------------------------------------------------
-- Utf8ToText
----------------------------------------------------------------------------

class Utf8ToText a where
    -- | Decode UTF8-encoded text to a 'Text'.
    --
    -- Malformed characters are replaced by @\\0xFFFD@ (the Unicode
    -- replacement character).
    utf8ToText :: a -> Text

instance Utf8ToText BS.ByteString where
    utf8ToText = decodeUtf8With lenientDecode
    {-# INLINE utf8ToText #-}

instance Utf8ToText BSL.ByteString where
    utf8ToText = decodeUtf8With lenientDecode . BSL.toStrict
    {-# INLINE utf8ToText #-}

----------------------------------------------------------------------------
-- Utf8ToLazyText
----------------------------------------------------------------------------

class Utf8ToLazyText a where
    -- | Decode UTF8-encoded text to a lazy 'Text'.
    --
    -- Malformed characters are replaced by @\\0xFFFD@ (the Unicode
    -- replacement character).
    utf8ToLazyText :: a -> TL.Text

instance Utf8ToLazyText BS.ByteString where
    utf8ToLazyText = TL.fromStrict . decodeUtf8With lenientDecode
    {-# INLINE utf8ToLazyText #-}

instance Utf8ToLazyText BSL.ByteString where
    utf8ToLazyText = TL.decodeUtf8With lenientDecode
    {-# INLINE utf8ToLazyText #-}

----------------------------------------------------------------------------
-- Utf8ToLazyText
----------------------------------------------------------------------------

class Utf8ToBuilder a where
    -- | Decode UTF8-encoded text to a 'Builder'.
    --
    -- Malformed characters are replaced by @\\0xFFFD@ (the Unicode
    -- replacement character).
    utf8ToBuilder :: a -> B.Builder

instance Utf8ToBuilder BS.ByteString where
    utf8ToBuilder = B.fromText . decodeUtf8With lenientDecode
    {-# INLINE utf8ToBuilder #-}

instance Utf8ToBuilder BSL.ByteString where
    utf8ToBuilder = B.fromLazyText . TL.decodeUtf8With lenientDecode
    {-# INLINE utf8ToBuilder #-}
