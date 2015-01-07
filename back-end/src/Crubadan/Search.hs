module Crubadan.Search ( genResults ) where

import Text.Regex(Regex, mkRegex, matchRegex)
import Data.Maybe(isJust)
import qualified Data.Map as M

import qualified Crubadan.Types as C
import qualified Crubadan.Shared.Types as CS

genResults :: CS.Query -> C.Database -> [CS.Result]
genResults q = let trans (key,val) = ( key, (mayRegex val) )
               in crunch q . filter (matches (fmap trans q)) 

mayRegex "" = Nothing
mayRegex s = Just (mkRegex s)

matches :: [(String, Maybe Regex)] -> C.WS -> Bool
matches r ws = (and . fmap isJust . fmap (matchAttr ws)) r

matchAttr :: C.WS -> (String, Maybe Regex) -> Maybe [String]
matchAttr ws (s, Nothing) = Just ["pass"]
matchAttr ws (s,(Just r)) = 
  M.lookup s (C.wsData ws) >>= matchRegex r . C.wsaMatch

crunch :: CS.Query -> [C.WS] -> [CS.Result]
crunch q = foldr (\w -> (:) (C.wsUID w, pullVals q w)) []

pullVals :: CS.Query -> C.WS -> [Maybe String]
pullVals q w = 
  let fetch k = (:) (M.lookup k (C.wsData w) 
                     >>= \a -> return (C.wsaFace a))
  in foldr fetch [] (fmap fst q)
