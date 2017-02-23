{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | A @CSV@ empty datatype with `MimeRender` and `MimeUnrender` instances for
-- @cassava@'s encoding and decoding classes.
--
-- >>> type Eg = Get '[(CSV', MyEncodeOptions)] [(Int, String)]
--
-- Default encoding and decoding options are also provided, along with the
-- @CSV@ type synonym that uses them.
--
-- >>> type EgDefault = Get '[CSV] [(Int, String)]
module Servant.CSV.Cassava where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative        ((<$>))
#endif
import           Data.Csv
import           Data.Proxy         (Proxy (..))
import           Data.Typeable      (Typeable)
import           Data.Vector        (Vector, toList)
import           GHC.Generics       (Generic)
import qualified Network.HTTP.Media as M
import           Servant.API        (Accept (..), MimeRender (..),
                                     MimeUnrender (..))

data CSVwith(a :: HasHeader) deriving (Typeable, Generic)

type CSV' = CSVwith 'HasHeader
type CSVNoHeader = CSVwith 'NoHeader

type CSV = (CSV', DefaultOpts)

data DefaultOpts deriving (Typeable, Generic)

-- | @text/csv;charset=utf-8@
instance Accept (CSVwith x, a) where
    contentType _ = "text" M.// "csv" M./: ("charset", "utf-8")

-- * Encoding

-- ** Instances

-- | Encode with 'encodeByNameWith'. The 'Header' param is used for determining
-- the order of headers and fields.
instance ( ToNamedRecord a, EncodeOpts opt
         ) => MimeRender (CSV', opt) (Header, [a]) where
    mimeRender _ (hdr, vals) = encodeByNameWith (encodeOpts p) hdr vals
      where p = Proxy :: Proxy opt

-- | Encode with 'encodeDefaultOrderedByNameWith'
instance ( DefaultOrdered a, ToNamedRecord a, EncodeOpts opt
         ) => MimeRender (CSV', opt) [a] where
    mimeRender _ = encodeDefaultOrderedByNameWith (encodeOpts p)
      where p = Proxy :: Proxy opt

-- | Encode with 'encodeWith' - but no header!
instance (ToRecord a, EncodeOpts opt
         ) => MimeRender (CSVNoHeader, opt) [a] where
    mimeRender _ = encodeWith ((encodeOpts p){encIncludeHeader = False})
      where p = Proxy :: Proxy opt

-- | Encode with 'encodeByNameWith'. The 'Header' param is used for determining
-- the order of headers and fields.
instance ( ToNamedRecord a, EncodeOpts opt
         ) => MimeRender (CSV', opt) (Header, Vector a) where
    mimeRender _ (hdr, vals) = encodeByNameWith (encodeOpts p) hdr (toList vals)
      where p = Proxy :: Proxy opt

-- | Encode with 'encodeDefaultOrderedByNameWith'
instance ( DefaultOrdered a, ToNamedRecord a, EncodeOpts opt
         ) => MimeRender (CSV', opt) (Vector a) where
    mimeRender _ = encodeDefaultOrderedByNameWith (encodeOpts p) . toList
      where p = Proxy :: Proxy opt

-- | Encode with 'encodeWith' - but no header
instance (ToRecord a, EncodeOpts opt
         ) => MimeRender (CSVNoHeader, opt) (Vector a) where
    mimeRender _ = encodeWith ((encodeOpts p){encIncludeHeader = False}) . toList
      where p = Proxy :: Proxy opt

-- ** Encode Options

class EncodeOpts a where
    encodeOpts :: Proxy a -> EncodeOptions


instance EncodeOpts DefaultOpts where
    encodeOpts _ = defaultEncodeOptions

-- * Decoding

-- ** Instances

-- | Decode with 'decodeByNameWith'
instance ( FromNamedRecord a, DecodeOpts opt
         ) => MimeUnrender (CSV', opt) (Header, [a]) where
    mimeUnrender _ bs = fmap toList <$> decodeByNameWith (decodeOpts p) bs
      where p = Proxy :: Proxy opt

-- | Decode with 'decodeWith', strips the headers
instance ( FromRecord a, DecodeOpts opt
         ) => MimeUnrender (CSV', opt) [a] where
    mimeUnrender _ bs = toList <$> decodeWith (decodeOpts p) HasHeader bs
      where p = Proxy :: Proxy opt

-- | Decode with 'decodeWith', for CSV without headers
instance ( FromRecord a, DecodeOpts opt
         ) => MimeUnrender (CSVNoHeader, opt) [a] where
    mimeUnrender _ bs = toList <$> decodeWith (decodeOpts p) NoHeader bs
      where p = Proxy :: Proxy opt

instance ( FromNamedRecord a, DecodeOpts opt
         ) => MimeUnrender (CSV', opt) (Header, Vector a) where
    mimeUnrender _ = decodeByNameWith (decodeOpts p)
      where p = Proxy :: Proxy opt

-- | Decode with 'decodeWith'. Assumes data has headers, which are stripped.
instance ( FromRecord a, DecodeOpts opt
         ) => MimeUnrender (CSV', opt) (Vector a) where
    mimeUnrender _ = decodeWith (decodeOpts p) HasHeader
      where p = Proxy :: Proxy opt

-- | Decode with 'decodeWith', for CSV without headers
instance ( FromRecord a, DecodeOpts opt
         ) => MimeUnrender (CSVNoHeader, opt) (Vector a) where
    mimeUnrender _ = decodeWith (decodeOpts p) NoHeader
      where p = Proxy :: Proxy opt

-- ** Decode Options

class DecodeOpts a where
    decodeOpts :: Proxy a -> DecodeOptions

instance DecodeOpts DefaultOpts where
    decodeOpts _ = defaultDecodeOptions
