{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module To
(
    -- * Conversion to 'String'
    ToString(..),
    Utf8ToString(..),

    -- * Conversion to strict 'Text'
    ToText(..),
    Utf8ToText(..),

    -- * Conversion to lazy 'Text'
    ToLazyText(..),
    Utf8ToLazyText(..),

    -- * Conversion to text 'TB.Builder'
    ToTextBuilder(..),
    Utf8ToTextBuilder(..),

    -- * Conversion to strict 'ByteString'
    ToByteString(..),
    ToUtf8ByteString(..),

    -- * Conversion to lazy 'ByteString'
    ToLazyByteString(..),
    ToUtf8LazyByteString(..),
)
where

import GHC.TypeLits (TypeError, ErrorMessage(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Lazy.Builder as TB
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
    -- | Transforming to strict 'T.Text'.
    toText :: a -> T.Text

instance (a ~ Char) => ToText [a] where
    toText = T.pack
    {-# INLINE toText #-}

instance ToText TL.Text where
    toText = TL.toStrict
    {-# INLINE toText #-}

instance ToText TB.Builder where
    toText = TL.toStrict . TB.toLazyText
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
    -- | Transforming to lazy 'TL.Text'.
    toLazyText :: a -> TL.Text

instance (a ~ Char) => ToLazyText [a] where
    toLazyText = TL.pack
    {-# INLINE toLazyText #-}

instance ToLazyText T.Text where
    toLazyText = TL.fromStrict
    {-# INLINE toLazyText #-}

instance ToLazyText TB.Builder where
    toLazyText = TB.toLazyText
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
-- ToTextBuilder
----------------------------------------------------------------------------

class ToTextBuilder a where
    -- | Transforming to text 'TB.Builder'.
    toTextBuilder :: a -> TB.Builder

instance (a ~ Char) => ToTextBuilder [a] where
    toTextBuilder = TB.fromString
    {-# INLINE toTextBuilder #-}

instance ToTextBuilder T.Text where
    toTextBuilder = TB.fromText
    {-# INLINE toTextBuilder #-}

instance ToTextBuilder TL.Text where
    toTextBuilder = TB.fromLazyText
    {-# INLINE toTextBuilder #-}

instance TypeError
    ('Text "Can not decode a ByteString without specifying encoding." :$$:
     'Text "Use 'utf8ToTextBuilder' if you want to decode ASCII or UTF8.")
    =>
    ToTextBuilder BS.ByteString where
    toTextBuilder = error "unreachable"

instance TypeError
    ('Text "Can not decode a lazy ByteString without specifying encoding." :$$:
     'Text "Use 'utf8ToTextBuilder' if you want to decode ASCII or UTF8.")
    =>
    ToTextBuilder BSL.ByteString where
    toTextBuilder = error "unreachable"

----------------------------------------------------------------------------
-- ToString
----------------------------------------------------------------------------

class ToString a where
    -- | Transforming to 'String'.
    toString :: a -> String

instance ToString T.Text where
    toString = T.unpack
    {-# INLINE toString #-}

instance ToString TL.Text where
    toString = TL.unpack
    {-# INLINE toString #-}

instance ToString TB.Builder where
    toString = TL.unpack . TB.toLazyText
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
    -- | Transforming to strict 'BS.ByteString'.
    toByteString :: a -> BS.ByteString

instance TypeError
    ('Text "Can not encode a Text without specifying encoding." :$$:
     'Text "Use 'toUtf8ByteString' if you want to encode as UTF8.")
    =>
    ToByteString T.Text where
    toByteString = error "unreachable"

instance TypeError
    ('Text "Can not encode a lazy Text without specifying encoding." :$$:
     'Text "Use 'toUtf8ByteString' if you want to encode as UTF8.")
    =>
    ToByteString TL.Text where
    toByteString = error "unreachable"

instance TypeError
    ('Text "Can not encode a text Builder without specifying encoding." :$$:
     'Text "Use 'toUtf8ByteString' if you want to encode as UTF8.")
    =>
    ToByteString TB.Builder where
    toByteString = error "unreachable"

instance (a ~ Char, TypeError
    ('Text "Can not encode a String without specifying encoding." :$$:
     'Text "Use 'toUtf8ByteString' if you want to encode as UTF8."))
    =>
    ToByteString [a] where
    toByteString = error "unreachable"

instance ToByteString BSL.ByteString where
    toByteString = BSL.toStrict
    {-# INLINE toByteString #-}

----------------------------------------------------------------------------
-- ToLazyByteString
----------------------------------------------------------------------------

class ToLazyByteString a where
    -- | Transforming to lazy 'BSL.ByteString'.
    toLazyByteString :: a -> BSL.ByteString

instance TypeError
    ('Text "Can not encode a Text without specifying encoding." :$$:
     'Text "Use 'toUtf8LazyByteString' if you want to encode as UTF8.")
    =>
    ToLazyByteString T.Text where
    toLazyByteString = error "unreachable"

instance TypeError
    ('Text "Can not encode a lazy Text without specifying encoding." :$$:
     'Text "Use 'toUtf8LazyByteString' if you want to encode as UTF8.")
    =>
    ToLazyByteString TL.Text where
    toLazyByteString = error "unreachable"

instance TypeError
    ('Text "Can not encode a text Builder without specifying encoding." :$$:
     'Text "Use 'toUtf8LazyByteString' if you want to encode as UTF8.")
    =>
    ToLazyByteString TB.Builder where
    toLazyByteString = error "unreachable"

instance (a ~ Char, TypeError
    ('Text "Can not encode a String without specifying encoding." :$$:
     'Text "Use 'toUtf8LazyByteString' if you want to encode as UTF8."))
    =>
    ToLazyByteString [a] where
    toLazyByteString = error "unreachable"

instance ToLazyByteString BS.ByteString where
    toLazyByteString = BSL.fromStrict
    {-# INLINE toLazyByteString #-}

----------------------------------------------------------------------------
-- Utf8ToString
----------------------------------------------------------------------------

class Utf8ToString a where
    -- | Decode UTF8-encoded text to a 'String'.
    --
    -- Malformed characters are replaced by @U+FFFD@ (the Unicode
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
    -- | Decode UTF8-encoded text to a strict 'T.Text'.
    --
    -- Malformed characters are replaced by @U+FFFD@ (the Unicode
    -- replacement character).
    utf8ToText :: a -> T.Text

instance Utf8ToText BS.ByteString where
    utf8ToText = T.decodeUtf8With T.lenientDecode
    {-# INLINE utf8ToText #-}

instance Utf8ToText BSL.ByteString where
    utf8ToText = T.decodeUtf8With T.lenientDecode . BSL.toStrict
    {-# INLINE utf8ToText #-}

----------------------------------------------------------------------------
-- Utf8ToLazyText
----------------------------------------------------------------------------

class Utf8ToLazyText a where
    -- | Decode UTF8-encoded text to a lazy 'Text'.
    --
    -- Malformed characters are replaced by @U+FFFD@ (the Unicode
    -- replacement character).
    utf8ToLazyText :: a -> TL.Text

instance Utf8ToLazyText BS.ByteString where
    utf8ToLazyText = TL.fromStrict . T.decodeUtf8With T.lenientDecode
    {-# INLINE utf8ToLazyText #-}

instance Utf8ToLazyText BSL.ByteString where
    utf8ToLazyText = TL.decodeUtf8With T.lenientDecode
    {-# INLINE utf8ToLazyText #-}

----------------------------------------------------------------------------
-- Utf8ToLazyText
----------------------------------------------------------------------------

class Utf8ToTextBuilder a where
    -- | Decode UTF8-encoded text to a text 'TB.Builder'.
    --
    -- Malformed characters are replaced by @U+FFFD@ (the Unicode
    -- replacement character).
    utf8ToTextBuilder :: a -> TB.Builder

instance Utf8ToTextBuilder BS.ByteString where
    utf8ToTextBuilder = TB.fromText . T.decodeUtf8With T.lenientDecode
    {-# INLINE utf8ToTextBuilder #-}

instance Utf8ToTextBuilder BSL.ByteString where
    utf8ToTextBuilder = TB.fromLazyText . TL.decodeUtf8With T.lenientDecode
    {-# INLINE utf8ToTextBuilder #-}

----------------------------------------------------------------------------
-- ToUtf8ByteString
----------------------------------------------------------------------------

class ToUtf8ByteString a where
    -- | UTF8-encode text to a 'BS.ByteString'.
    toUtf8ByteString :: a -> BS.ByteString

instance ToUtf8ByteString T.Text where
    toUtf8ByteString = T.encodeUtf8
    {-# INLINE toUtf8ByteString #-}

instance ToUtf8ByteString TL.Text where
    toUtf8ByteString = T.encodeUtf8 . TL.toStrict
    {-# INLINE toUtf8ByteString #-}

instance ToUtf8ByteString TB.Builder where
    toUtf8ByteString = T.encodeUtf8 . TL.toStrict . TB.toLazyText
    {-# INLINE toUtf8ByteString #-}

instance (a ~ Char) => ToUtf8ByteString [a] where
    toUtf8ByteString = UTF8.fromString
    {-# INLINE toUtf8ByteString #-}

----------------------------------------------------------------------------
-- ToUtf8LazyByteString
----------------------------------------------------------------------------

class ToUtf8LazyByteString a where
    -- | UTF8-encode text to a lazy 'BSL.ByteString'.
    toUtf8LazyByteString :: a -> BSL.ByteString

instance ToUtf8LazyByteString T.Text where
    toUtf8LazyByteString = TL.encodeUtf8 . TL.fromStrict
    {-# INLINE toUtf8LazyByteString #-}

instance ToUtf8LazyByteString TL.Text where
    toUtf8LazyByteString = TL.encodeUtf8
    {-# INLINE toUtf8LazyByteString #-}

instance ToUtf8LazyByteString TB.Builder where
    toUtf8LazyByteString = TL.encodeUtf8 . TB.toLazyText
    {-# INLINE toUtf8LazyByteString #-}

instance (a ~ Char) => ToUtf8LazyByteString [a] where
    toUtf8LazyByteString = UTF8L.fromString
    {-# INLINE toUtf8LazyByteString #-}
