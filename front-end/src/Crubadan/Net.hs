module Crubadan.Net ( netget ) where

import Data.Maybe (fromJust)
import Data.Default (def)
import JavaScript.JQuery (ajax, arData)

import qualified Data.Text as T (pack, unpack, Text)

import qualified Crubadan.Types as CR (Database)

url = T.pack "http://octalsrc.net/cgi/"

query q = [(T.pack "query", T.pack (show q))]

netget :: String -> IO (Maybe CR.Database)
netget r = do ar <- ajax url (query r) def
              return . fmap (fromJust . read . T.unpack) . arData $ ar
