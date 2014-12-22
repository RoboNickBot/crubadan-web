module Crubadan.Search ( genResults ) where

import Text.Regex(Regex, mkRegex, matchRegex)
import Data.Maybe(isJust)
import qualified Data.Map as M

import qualified Crubadan.Types as C

genResults :: C.Query -> C.Database -> C.Result
genResults q = let trans (key,val) = ( key, (mkRegex val) )
               in filter (matches (fmap trans q)) 

matches :: [(String, Regex)] -> C.WS -> Bool
matches r ws = (and . fmap isJust . fmap (matchAttr ws)) r

matchAttr :: C.WS -> (String, Regex) -> Maybe [String]
matchAttr ws (s,r) = M.lookup s (C.wsData ws) >>= matchRegex r . C.wsaMatch
