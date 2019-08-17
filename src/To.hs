{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module To
(
    -- * Maps and sets
    -- ** 'ML.Map'
    ToMap(..),
    -- ** 'S.Set'
    ToSet(..),
    -- ** 'IML.IntMap'
    ToIntMap(..),
    -- ** 'IS.IntSet'
    ToIntSet(..),
    -- ** 'HML.HashMap'
    ToHashMap(..),
    -- ** 'HS.HashSet'
    ToHashSet(..),

    -- * Strings and bytestrings
    -- ** 'String'
    ToString(..),
    Utf8ToString(..),
    -- ** Strict 'T.Text'
    ToText(..),
    Utf8ToText(..),
    -- ** Lazy 'TL.Text'
    ToLazyText(..),
    Utf8ToLazyText(..),
    -- ** Text 'TB.Builder'
    ToTextBuilder(..),
    Utf8ToTextBuilder(..),
    -- ** Strict 'BS.ByteString'
    ToByteString(..),
    ToUtf8ByteString(..),
    -- ** Lazy 'BSL.ByteString'
    ToLazyByteString(..),
    ToUtf8LazyByteString(..),
)
where

import GHC.TypeLits (TypeError, ErrorMessage(..))
import Data.Hashable
import qualified Data.Map.Lazy as ML
import qualified Data.IntMap.Lazy as IML
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.HashMap.Lazy as HML
import qualified Data.HashSet as HS
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
-- ToMap
----------------------------------------------------------------------------

class ToMap a k v | a -> k v, a k -> v, a v -> k where
    -- | Turn into a 'ML.Map'.
    toMap :: a -> ML.Map k v

instance (kv ~ (k, v), Ord k) => ToMap [kv] k v where
    toMap = ML.fromList
    {-# INLINE toMap #-}

instance ToMap (IML.IntMap v) Int v where
    toMap = ML.fromDistinctAscList . IML.toAscList
    {-# INLINE toMap #-}

instance Ord k => ToMap (HML.HashMap k v) k v where
    toMap = HML.foldlWithKey' (\m k v -> ML.insert k v m) mempty
    {-# INLINE toMap #-}

----------------------------------------------------------------------------
-- ToSet
----------------------------------------------------------------------------

class ToSet a k | a -> k where
    -- | Turn into a 'S.Set'.
    toSet :: a -> S.Set k

instance Ord k => ToSet [k] k where
    toSet = S.fromList
    {-# INLINE toSet #-}

instance ToSet IS.IntSet Int where
    toSet = S.fromDistinctAscList . IS.toAscList
    {-# INLINE toSet #-}

instance Ord k => ToSet (HS.HashSet k) k where
    toSet = HS.foldl' (flip S.insert) mempty
    {-# INLINE toSet #-}

----------------------------------------------------------------------------
-- ToIntMap
----------------------------------------------------------------------------

class ToIntMap a v | a -> v where
    -- | Turn into an 'IML.IntMap'.
    toIntMap :: a -> IML.IntMap v

instance (kv ~ (Int, v)) => ToIntMap [kv] v where
    toIntMap = IML.fromList
    {-# INLINE toIntMap #-}

instance ToIntMap (ML.Map Int v) v where
    toIntMap = IML.fromDistinctAscList . ML.toAscList
    {-# INLINE toIntMap #-}

instance ToIntMap (HML.HashMap Int v) v where
    toIntMap = HML.foldlWithKey' (\m k v -> IML.insert k v m) mempty
    {-# INLINE toIntMap #-}

----------------------------------------------------------------------------
-- ToIntSet
----------------------------------------------------------------------------

class ToIntSet a where
    -- | Turn into an 'IS.IntSet'.
    toIntSet :: a -> IS.IntSet

instance (k ~ Int) => ToIntSet [k] where
    toIntSet = IS.fromList
    {-# INLINE toIntSet #-}

instance ToIntSet (S.Set Int) where
    toIntSet = IS.fromDistinctAscList . S.toAscList
    {-# INLINE toIntSet #-}

instance ToIntSet (HS.HashSet Int) where
    toIntSet = HS.foldl' (flip IS.insert) mempty
    {-# INLINE toIntSet #-}

----------------------------------------------------------------------------
-- ToHashMap
----------------------------------------------------------------------------

class ToHashMap a k v | a -> k v, a k -> v, a v -> k where
    -- | Turn into a 'HML.HashMap'.
    toHashMap :: a -> HML.HashMap k v

instance (kv ~ (k, v), Eq k, Hashable k) => ToHashMap [kv] k v where
    toHashMap = HML.fromList
    {-# INLINE toHashMap #-}

instance (Eq k, Hashable k) => ToHashMap (ML.Map k v) k v where
    toHashMap = ML.foldlWithKey' (\m k v -> HML.insert k v m) mempty
    {-# INLINE toHashMap #-}

instance ToHashMap (IML.IntMap v) Int v where
    toHashMap = IML.foldlWithKey' (\m k v -> HML.insert k v m) mempty
    {-# INLINE toHashMap #-}

----------------------------------------------------------------------------
-- ToHashSet
----------------------------------------------------------------------------

class ToHashSet a k | a -> k where
    -- | Turn into a 'HS.HashSet'.
    toHashSet :: a -> HS.HashSet k

instance (Eq k, Hashable k) => ToHashSet [k] k where
    toHashSet = HS.fromList
    {-# INLINE toHashSet #-}

instance (Eq k, Hashable k) => ToHashSet (S.Set k) k where
    toHashSet = S.foldl' (flip HS.insert) mempty
    {-# INLINE toHashSet #-}

instance ToHashSet IS.IntSet Int where
    toHashSet = IS.foldl' (flip HS.insert) mempty
    {-# INLINE toHashSet #-}

----------------------------------------------------------------------------
-- ToText
----------------------------------------------------------------------------

class ToText a where
    -- | Turn into strict 'T.Text'.
    toText :: a -> T.Text

-- | 'String'
instance (a ~ Char) => ToText [a] where
    toText = T.pack
    {-# INLINE toText #-}

instance ToText TL.Text where
    toText = TL.toStrict
    {-# INLINE toText #-}

instance ToText TB.Builder where
    toText = TL.toStrict . TB.toLazyText
    {-# INLINE toText #-}

-- | Use 'utf8ToText'.
instance TypeError (SpecifyDecoding BS.ByteString "utf8ToText") =>
         ToText BS.ByteString where
    toText = error "unreachable"

-- | Use 'utf8ToText'.
instance TypeError (SpecifyDecoding BSL.ByteString "utf8ToText") =>
         ToText BSL.ByteString where
    toText = error "unreachable"

----------------------------------------------------------------------------
-- ToLazyText
----------------------------------------------------------------------------

class ToLazyText a where
    -- | Turn into lazy 'TL.Text'.
    toLazyText :: a -> TL.Text

-- | 'String'
instance (a ~ Char) => ToLazyText [a] where
    toLazyText = TL.pack
    {-# INLINE toLazyText #-}

instance ToLazyText T.Text where
    toLazyText = TL.fromStrict
    {-# INLINE toLazyText #-}

instance ToLazyText TB.Builder where
    toLazyText = TB.toLazyText
    {-# INLINE toLazyText #-}

-- | Use 'utf8ToLazyText'.
instance TypeError (SpecifyDecoding BS.ByteString "utf8ToLazyText") =>
         ToLazyText BS.ByteString where
    toLazyText = error "unreachable"

-- | Use 'utf8ToLazyText'.
instance TypeError (SpecifyDecoding BSL.ByteString "utf8ToLazyText") =>
         ToLazyText BSL.ByteString where
    toLazyText = error "unreachable"

----------------------------------------------------------------------------
-- ToTextBuilder
----------------------------------------------------------------------------

class ToTextBuilder a where
    -- | Turn into text 'TB.Builder'.
    toTextBuilder :: a -> TB.Builder

-- | 'String'
instance (a ~ Char) => ToTextBuilder [a] where
    toTextBuilder = TB.fromString
    {-# INLINE toTextBuilder #-}

instance ToTextBuilder T.Text where
    toTextBuilder = TB.fromText
    {-# INLINE toTextBuilder #-}

instance ToTextBuilder TL.Text where
    toTextBuilder = TB.fromLazyText
    {-# INLINE toTextBuilder #-}

-- | Use 'utf8ToTextBuilder'.
instance TypeError (SpecifyDecoding BS.ByteString "utf8ToTextBuilder") =>
         ToTextBuilder BS.ByteString where
    toTextBuilder = error "unreachable"

-- | Use 'utf8ToTextBuilder'.
instance TypeError (SpecifyDecoding BSL.ByteString "utf8ToTextBuilder") =>
         ToTextBuilder BSL.ByteString where
    toTextBuilder = error "unreachable"

----------------------------------------------------------------------------
-- ToString
----------------------------------------------------------------------------

class ToString a where
    -- | Turn into 'String'.
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

-- | Use 'utf8ToString'.
instance TypeError (SpecifyDecoding BS.ByteString "utf8ToString") =>
         ToString BS.ByteString where
    toString = error "unreachable"

-- | Use 'utf8ToString'.
instance TypeError (SpecifyDecoding BSL.ByteString "utf8ToString") =>
         ToString BSL.ByteString where
    toString = error "unreachable"

----------------------------------------------------------------------------
-- ToByteString
----------------------------------------------------------------------------

class ToByteString a where
    -- | Turn into strict 'BS.ByteString'.
    toByteString :: a -> BS.ByteString

-- | Use 'toUtf8ByteString'.
instance TypeError (SpecifyEncoding T.Text "toUtf8ByteString") =>
         ToByteString T.Text where
    toByteString = error "unreachable"

-- | Use 'toUtf8ByteString'.
instance TypeError (SpecifyEncoding TL.Text "toUtf8ByteString") =>
         ToByteString TL.Text where
    toByteString = error "unreachable"

-- | Use 'toUtf8ByteString'.
instance TypeError (SpecifyEncoding TB.Builder "toUtf8ByteString") =>
         ToByteString TB.Builder where
    toByteString = error "unreachable"

-- | Use 'toUtf8ByteString'.
instance (a ~ Char, TypeError (SpecifyEncoding String "toUtf8ByteString")) =>
         ToByteString [a] where
    toByteString = error "unreachable"

-- | Use 'toUtf8ByteString'.
instance ToByteString BSL.ByteString where
    toByteString = BSL.toStrict
    {-# INLINE toByteString #-}

----------------------------------------------------------------------------
-- ToLazyByteString
----------------------------------------------------------------------------

class ToLazyByteString a where
    -- | Turn into lazy 'BSL.ByteString'.
    toLazyByteString :: a -> BSL.ByteString

-- | Use 'toUtf8LazyByteString'.
instance TypeError (SpecifyEncoding T.Text "toUtf8LazyByteString") =>
         ToLazyByteString T.Text where
    toLazyByteString = error "unreachable"

-- | Use 'toUtf8LazyByteString'.
instance TypeError (SpecifyEncoding TL.Text "toUtf8LazyByteString") =>
         ToLazyByteString TL.Text where
    toLazyByteString = error "unreachable"

-- | Use 'toUtf8LazyByteString'.
instance TypeError (SpecifyEncoding TB.Builder "toUtf8LazyByteString") =>
         ToLazyByteString TB.Builder where
    toLazyByteString = error "unreachable"

-- | Use 'toUtf8LazyByteString'.
instance (a ~ Char, TypeError (SpecifyEncoding String "toUtf8LazyByteString")) =>
         ToLazyByteString [a] where
    toLazyByteString = error "unreachable"

instance ToLazyByteString BS.ByteString where
    toLazyByteString = BSL.fromStrict
    {-# INLINE toLazyByteString #-}

----------------------------------------------------------------------------
-- Utf8ToString
----------------------------------------------------------------------------

class Utf8ToString a where
    -- | Decode UTF8-encoded text into 'String'.
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
    -- | Decode UTF8-encoded text into strict 'T.Text'.
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
    -- | Decode UTF8-encoded text into lazy 'TL.Text'.
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
    -- | Decode UTF8-encoded text into text 'TB.Builder'.
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
    -- | UTF8-encode text into 'BS.ByteString'.
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

-- | 'String'
instance (a ~ Char) => ToUtf8ByteString [a] where
    toUtf8ByteString = UTF8.fromString
    {-# INLINE toUtf8ByteString #-}

----------------------------------------------------------------------------
-- ToUtf8LazyByteString
----------------------------------------------------------------------------

class ToUtf8LazyByteString a where
    -- | UTF8-encode text into lazy 'BSL.ByteString'.
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

-- | 'String'
instance (a ~ Char) => ToUtf8LazyByteString [a] where
    toUtf8LazyByteString = UTF8L.fromString
    {-# INLINE toUtf8LazyByteString #-}

----------------------------------------------------------------------------
-- Type errors
----------------------------------------------------------------------------

type SpecifyEncoding type_ proposed =
    'Text "Can not encode a " :<>: 'ShowType type_ :<>:
        'Text " without specifying encoding." :$$:
    'Text "Use '" :<>: 'Text proposed :<>:
        'Text "' if you want to encode as UTF8."

type SpecifyDecoding type_ proposed =
    'Text "Can not decode a " :<>: 'ShowType type_ :<>:
        'Text " without specifying encoding." :$$:
    'Text "Use '" :<>: 'Text proposed :<>:
        'Text "' if you want to decode ASCII or UTF8."
