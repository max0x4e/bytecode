{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

module GHC.Hs.Doc
  ( HsDocString
  , LHsDocString
  , mkHsDocString
  , mkHsDocStringUtf8ByteString
  , isEmptyDocString
  , unpackHDS
  , hsDocStringToByteString
  , ppr_mbDoc

  , appendDocs
  , concatDocs

  , DeclDocMap(..)
  , emptyDeclDocMap

  , ArgDocMap(..)
  , emptyArgDocMap

  , ExtractedTHDocs(..)
  ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Utils.Binary
import GHC.Utils.Encoding
import GHC.Types.Name
import GHC.Utils.Outputable as Outputable
import GHC.Types.SrcLoc

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Data
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

-- | Haskell Documentation String
--
-- Internally this is a UTF8-Encoded 'ByteString'.
newtype HsDocString = HsDocString ByteString
  -- There are at least two plausible Semigroup instances for this type:
  --
  -- 1. Simple string concatenation.
  -- 2. Concatenation as documentation paragraphs with newlines in between.
  --
  -- To avoid confusion, we pass on defining an instance at all.
  deriving (Eq, Show, Data)

-- | Located Haskell Documentation String
type LHsDocString = Located HsDocString

instance Binary HsDocString where
  put_ bh (HsDocString bs) = put_ bh bs
  get bh = HsDocString <$> get bh

instance Outputable HsDocString where
  ppr = doubleQuotes . text . unpackHDS

isEmptyDocString :: HsDocString -> Bool
isEmptyDocString (HsDocString bs) = BS.null bs

mkHsDocString :: String -> HsDocString
mkHsDocString s = HsDocString (utf8EncodeString s)

-- | Create a 'HsDocString' from a UTF8-encoded 'ByteString'.
mkHsDocStringUtf8ByteString :: ByteString -> HsDocString
mkHsDocStringUtf8ByteString = HsDocString

unpackHDS :: HsDocString -> String
unpackHDS = utf8DecodeByteString . hsDocStringToByteString

-- | Return the contents of a 'HsDocString' as a UTF8-encoded 'ByteString'.
hsDocStringToByteString :: HsDocString -> ByteString
hsDocStringToByteString (HsDocString bs) = bs

ppr_mbDoc :: Maybe LHsDocString -> SDoc
ppr_mbDoc (Just doc) = ppr doc
ppr_mbDoc Nothing    = empty

-- | Join two docstrings.
--
-- Non-empty docstrings are joined with two newlines in between,
-- resulting in separate paragraphs.
appendDocs :: HsDocString -> HsDocString -> HsDocString
appendDocs x y =
  fromMaybe
    (HsDocString BS.empty)
    (concatDocs [x, y])

-- | Concat docstrings with two newlines in between.
--
-- Empty docstrings are skipped.
--
-- If all inputs are empty, 'Nothing' is returned.
concatDocs :: [HsDocString] -> Maybe HsDocString
concatDocs xs =
    if BS.null b
      then Nothing
      else Just (HsDocString b)
  where
    b = BS.intercalate (C8.pack "\n\n")
      . filter (not . BS.null)
      . map hsDocStringToByteString
      $ xs

-- | Docs for declarations: functions, data types, instances, methods etc.
newtype DeclDocMap = DeclDocMap (Map Name HsDocString)

instance Binary DeclDocMap where
  put_ bh (DeclDocMap m) = put_ bh (Map.toList m)
  -- We can't rely on a deterministic ordering of the `Name`s here.
  -- See the comments on `Name`'s `Ord` instance for context.
  get bh = DeclDocMap . Map.fromList <$> get bh

instance Outputable DeclDocMap where
  ppr (DeclDocMap m) = vcat (map pprPair (Map.toAscList m))
    where
      pprPair (name, doc) = ppr name Outputable.<> colon $$ nest 2 (ppr doc)

emptyDeclDocMap :: DeclDocMap
emptyDeclDocMap = DeclDocMap Map.empty

-- | Docs for arguments. E.g. function arguments, method arguments.
newtype ArgDocMap = ArgDocMap (Map Name (IntMap HsDocString))

instance Binary ArgDocMap where
  put_ bh (ArgDocMap m) = put_ bh (Map.toList (IntMap.toAscList <$> m))
  -- We can't rely on a deterministic ordering of the `Name`s here.
  -- See the comments on `Name`'s `Ord` instance for context.
  get bh = ArgDocMap . fmap IntMap.fromDistinctAscList . Map.fromList <$> get bh

instance Outputable ArgDocMap where
  ppr (ArgDocMap m) = vcat (map pprPair (Map.toAscList m))
    where
      pprPair (name, int_map) =
        ppr name Outputable.<> colon $$ nest 2 (pprIntMap int_map)
      pprIntMap im = vcat (map pprIPair (IntMap.toAscList im))
      pprIPair (i, doc) = ppr i Outputable.<> colon $$ nest 2 (ppr doc)

emptyArgDocMap :: ArgDocMap
emptyArgDocMap = ArgDocMap Map.empty

-- | Maps of docs that were added via Template Haskell's @putDoc@.
data ExtractedTHDocs =
  ExtractedTHDocs
    { ethd_mod_header :: Maybe HsDocString
      -- ^ The added module header documentation, if it exists.
    , ethd_decl_docs  :: DeclDocMap
      -- ^ The documentation added to declarations.
    , ethd_arg_docs   :: ArgDocMap
      -- ^ The documentation added to function arguments.
    , ethd_inst_docs  :: DeclDocMap
      -- ^ The documentation added to class and family instances.
    }
