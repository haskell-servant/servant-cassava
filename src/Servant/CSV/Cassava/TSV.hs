{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE KindSignatures              #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE DeriveDataTypeable          #-}


-- | A @TSV@ (tab separated values) empty datatype with `MimeRender` and `MimeUnrender` instances for
-- @cassava@'s encoding and decoding classes. The media type in the `Accept` instance is "text/tab-separated-values".
--
-- >>> type Eg = Get '[TSV' 'HasHeader MyEncodeOptions] [(Int, String)]
--
-- Default encoding and decoding options are also provided, along with the
-- @TSV@ type synonym that uses them.
--
-- >>> type EgDefault = Get '[TSV] [(Int, String)]

module Servant.CSV.Cassava.TSV (TSV', TSV, TSVOpts, HasHeader(..)) where

import           Data.Char            (ord)
import           Data.Csv
import           Data.Proxy           (Proxy (..))
import           Data.Typeable        (Typeable)
import           Data.Vector          (Vector)
import qualified Network.HTTP.Media   as M
import           Servant.API          (Accept (..), MimeRender (..),
                                      MimeUnrender (..))
import           Servant.CSV.Cassava

-- Note we can't do this as an alias for CSV, because the Accept instance is for a different Content-Type.
data TSV' (hasHeader :: HasHeader) opt deriving (Typeable)
type TSV = TSV' 'HasHeader TSVOpts

swapProxy :: Proxy (TSV' h o) -> Proxy (CSV' h o)
swapProxy _ = Proxy

instance Accept (TSV' hasHeader opt) where
    contentType _ = "text" M.// "tab-separated-values" M./: ("charset", "utf-8")

instance ( ToNamedRecord a, EncodeOpts opt, SHasHeaderI hasHeader
         ) => MimeRender (TSV' hasHeader opt) (Header, [a])
    where mimeRender  = mimeRender . swapProxy

instance ( EncodeOpts opt, EncodeList hasHeader a
         ) => MimeRender (TSV' hasHeader opt) [a]
    where mimeRender  = mimeRender . swapProxy

instance ( ToNamedRecord a, EncodeOpts opt, SHasHeaderI hasHeader
         ) => MimeRender (TSV' hasHeader opt) (Header, Vector a)
    where mimeRender  = mimeRender . swapProxy

instance ( EncodeOpts opt, EncodeList hasHeader a
         ) => MimeRender (TSV' hasHeader opt) (Vector a)
    where mimeRender  = mimeRender . swapProxy

instance ( FromNamedRecord a, DecodeOpts opt
         ) => MimeUnrender (TSV' 'HasHeader opt) (Header, [a])
    where mimeUnrender = mimeUnrender . swapProxy

instance ( FromRecord a, DecodeOpts opt, SHasHeaderI hasHeader
         ) => MimeUnrender (TSV' hasHeader opt) [a]
    where mimeUnrender = mimeUnrender . swapProxy

instance ( FromNamedRecord a, DecodeOpts opt
         ) => MimeUnrender (TSV' 'HasHeader opt) (Header, Vector a)
    where mimeUnrender = mimeUnrender . swapProxy

instance ( FromRecord a, DecodeOpts opt, SHasHeaderI hasHeader
         ) => MimeUnrender (TSV' hasHeader opt) (Vector a)
    where mimeUnrender = mimeUnrender . swapProxy

data TSVOpts deriving (Typeable)

instance EncodeOpts TSVOpts where
    encodeOpts _ = defaultEncodeOptions {encDelimiter = fromIntegral (ord '\t')}

instance DecodeOpts TSVOpts where
    decodeOpts _ = defaultDecodeOptions {decDelimiter = fromIntegral (ord '\t')}