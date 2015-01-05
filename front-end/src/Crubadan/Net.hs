module Crubadan.Net ( netget ) where

import Data.Maybe (fromJust)
import Data.Default (def)
import JavaScript.JQuery (ajax, arData)

import qualified Data.Text as T (pack, unpack, Text)

import qualified Crubadan.Shared.Types as CS

url = T.pack "http://localhost/cgi/"   --"http://octalsrc.net/cgi/"

query q = [(T.pack "query", T.pack (show q))]

netget :: CS.Query -> IO (Maybe CS.Result)
netget r = do ar <- ajax url (query r) def
              return . fmap (fromJust . read . T.unpack) . arData $ ar
