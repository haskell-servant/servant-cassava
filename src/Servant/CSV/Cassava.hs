{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | A @CSV@ empty datatype with `MimeRender` and `MimeUnrender` instances for
-- @cassava@'s encoding and decoding classes.
--
-- >>> type Eg = Get '[CSV' 'HasHeader MyEncodeOptions] [(Int, String)]
--
-- Default encoding and decoding options are also provided, along with the
-- @CSV@ type synonym that uses them.
--
-- >>> type EgDefault = Get '[CSV] [(Int, String)]
--
module Servant.CSV.Cassava ( module Servant.CSV.Cassava
                           , HasHeader(..)
                           ) where

import           Prelude ()
import           Prelude.Compat 

import           Data.Csv
import           Data.ByteString.Lazy (ByteString)
import           Data.Proxy           (Proxy (..))
import           Data.Typeable        (Typeable)
import           Data.Vector          (Vector, toList)
import           GHC.Generics         (Generic)
import qualified Network.HTTP.Media   as M
import           Servant.API          (Accept (..), MimeRender (..),
                                      MimeUnrender (..))

data CSV' (hasHeader :: HasHeader) opt deriving (Typeable)
type CSV = CSV' 'HasHeader DefaultOpts

-- | 'HasHeader singleton.
data SHasHeader (hasHeader :: HasHeader) where
    SHasHeader  :: SHasHeader 'HasHeader
    SNoHeader   :: SHasHeader 'NoHeader

-- | Class to provide 'SHasHeader' implicitly.
class SHasHeaderI (hasHeader :: HasHeader) where shasheader :: SHasHeader hasHeader
instance SHasHeaderI 'HasHeader where shasheader = SHasHeader
instance SHasHeaderI 'NoHeader  where shasheader = SNoHeader

shasheaderToBool :: SHasHeader hasHeader -> Bool
shasheaderToBool SHasHeader = True
shasheaderToBool SNoHeader  = False

lowerSHasHeader :: SHasHeader hasHeader -> HasHeader
lowerSHasHeader SHasHeader = HasHeader
lowerSHasHeader SNoHeader  = NoHeader

-- | Default options, instances providing 'defaultDecodeOptions' and 'defaultEncodeOptions'.
data DefaultOpts deriving (Typeable, Generic)

-- | @text/csv;charset=utf-8@
instance Accept (CSV' hasHeader opt) where
    contentType _ = "text" M.// "csv" M./: ("charset", "utf-8")

-- * Encoding

-- ** Instances

-- | Encode with 'encodeByNameWith'. The 'Header' param is used for determining
-- the order of headers and fields.
instance ( ToNamedRecord a, EncodeOpts opt, SHasHeaderI hasHeader
         ) => MimeRender (CSV' hasHeader opt) (Header, [a]) where
    mimeRender _ (hdr, vals) = encodeByNameWith opts hdr vals
      where
        opts = encodeOpts' (Proxy :: Proxy opt) (Proxy :: Proxy hasHeader)

-- | A class to determine how to encode a list of elements
--
-- * 'HasHeader' encode with 'encodeDefaultOrderedByNameWith'
--
-- * 'NoHeader' encode with 'encodeWith'
--
-- Currently, it's not possible to encode without headers using 'encodeDefaultOrderedByNameWith'.
--
class EncodeList (hasHeader :: HasHeader) a where
    encodeList :: Proxy hasHeader -> EncodeOptions -> [a] -> ByteString

-- | 'encodeDefaultOrderedByNameWith'
instance (DefaultOrdered a, ToNamedRecord a) => EncodeList 'HasHeader a where
    encodeList _ opts vals = encodeDefaultOrderedByNameWith opts { encIncludeHeader = True } vals

-- | 'encodeWith'
instance (ToRecord a) => EncodeList 'NoHeader a where
    encodeList _ opts vals = encodeWith opts { encIncludeHeader = False } vals

instance ( EncodeOpts opt, EncodeList hasHeader a
         ) => MimeRender (CSV' hasHeader opt) [a] where
    mimeRender _ = encodeList (Proxy :: Proxy hasHeader) opts
      where
        opts = encodeOpts (Proxy :: Proxy opt)

-- | Encode with 'encodeByNameWith'. The 'Header' param is used for determining
-- the order of headers and fields.
instance ( ToNamedRecord a, EncodeOpts opt, SHasHeaderI hasHeader
         ) => MimeRender (CSV' hasHeader opt) (Header, Vector a) where
    mimeRender _ (hdr, vals) = encodeByNameWith opts hdr (toList vals)
      where
        opts = encodeOpts' (Proxy :: Proxy opt) (Proxy :: Proxy hasHeader)

instance ( EncodeOpts opt, EncodeList hasHeader a
         ) => MimeRender (CSV' hasHeader opt) (Vector a) where
    mimeRender _ = encodeList (Proxy :: Proxy hasHeader) opts . toList
      where
        opts = encodeOpts (Proxy :: Proxy opt)

-- ** Encode Options

class EncodeOpts opt where
    encodeOpts :: Proxy opt -> EncodeOptions

encodeOpts'
    :: forall opt hasHeader.  (EncodeOpts opt, SHasHeaderI hasHeader)
    => Proxy opt -> Proxy hasHeader -> EncodeOptions
encodeOpts' p _ = (encodeOpts p)
    { encIncludeHeader = shasheaderToBool (shasheader :: SHasHeader hasHeader)
    }

instance EncodeOpts DefaultOpts where
    encodeOpts _ = defaultEncodeOptions

-- * Decoding

-- ** Instances

-- | Decode with 'decodeByNameWith'.
instance ( FromNamedRecord a, DecodeOpts opt
         ) => MimeUnrender (CSV' 'HasHeader opt) (Header, [a]) where
    mimeUnrender _ bs = fmap toList <$> decodeByNameWith (decodeOpts p) bs
      where p = Proxy :: Proxy opt

-- | Decode with 'decodeWith'.
instance ( FromRecord a, DecodeOpts opt, SHasHeaderI hasHeader
         ) => MimeUnrender (CSV' hasHeader opt) [a] where
    mimeUnrender _  = fmap toList . decodeWith (decodeOpts p) (lowerSHasHeader sh)
      where
        p = Proxy :: Proxy opt
        sh = shasheader :: SHasHeader hasHeader

instance ( FromNamedRecord a, DecodeOpts opt
         ) => MimeUnrender (CSV' 'HasHeader opt) (Header, Vector a) where
    mimeUnrender _ = decodeByNameWith (decodeOpts p)
      where p = Proxy :: Proxy opt

-- | Decode with 'decodeWith'.
instance ( FromRecord a, DecodeOpts opt, SHasHeaderI hasHeader
         ) => MimeUnrender (CSV' hasHeader opt) (Vector a) where
    mimeUnrender _ = decodeWith (decodeOpts p) (lowerSHasHeader sh)
      where
        p = Proxy :: Proxy opt
        sh = shasheader :: SHasHeader hasHeader


-- ** Decode Options

class DecodeOpts a where
    decodeOpts :: Proxy a -> DecodeOptions

instance DecodeOpts DefaultOpts where
    decodeOpts _ = defaultDecodeOptions
