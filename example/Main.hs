{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TypeOperators   #-}
module Main (main, Foo (..)) where

import Prelude        ()
import Prelude.Compat

import Data.Csv           (DefaultOrdered, FromRecord, ToNamedRecord, ToRecord)
import Data.Maybe         (fromMaybe)
import GHC.Generics       (Generic)
import Network.Wai        (Application)
import Servant
import Servant.CSV.Cassava
import System.Environment (getArgs, lookupEnv)
import Text.Read          (readMaybe)

import qualified Network.Wai.Handler.Warp as Warp

data Foo = Foo
    { fooName :: String
    , fooNum  :: Int
    }
  deriving (Eq, Show, Generic)

instance DefaultOrdered Foo
instance FromRecord Foo
instance ToNamedRecord Foo
instance ToRecord Foo

type API = "post" :> ReqBody '[CSV] [Foo] :> Post '[CSV] [Foo]
      :<|> "csv" :> Get '[CSV] [Foo]
      :<|> "tsv" :> Get '[CSV' 'NoHeader TabSeparatedOpts] [Foo]

api :: Proxy API
api = Proxy

server :: Server API
server = pure :<|> pure [Foo "Csv!" 42] :<|> pure [Foo "Tsv!" 7]

app :: Application
app = serve api server

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("run":_) -> do
            port <- fromMaybe 8000 . (>>= readMaybe) <$> lookupEnv "PORT"
            Warp.run port app
        _ -> putStrLn "To run, pass run argument"
