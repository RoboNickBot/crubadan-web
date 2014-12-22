module Crubadan.Types ( Query
                      , Result
                      , Database
                      , WS (..)
                      , Attribute (..) ) where

import qualified Data.Map as M

type Query = [(String, String)]
type Result = [WS]

type Database = [WS]

data WS = WS { wsUID :: String
             , wsData :: M.Map String Attribute } deriving (Show, Eq)

data Attribute = Attribute { wsaFace :: String
                           , wsaMatch :: String } deriving (Show, Eq)
