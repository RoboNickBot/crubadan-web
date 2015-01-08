module Crubadan.Shared.Types ( Query
                             , Result
                             , resultID
                             , resultData ) where

type Query = [(String, String)]

type Result = (String, [Maybe String])

resultID :: Result -> String
resultID = fst
resultData :: Result -> [Maybe String]
resultData = snd
