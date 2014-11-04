module Crubadan.Search ( genResults ) where

database :: [String]
database = ["abc","eng","aak","arc","src","nsa","usa","cat","sci","slu","edu"]

-- Once the database can be generated from the filesystem with\
-- an IO function, genResults will take the query string AND the\
-- database as args
genResults :: String -> [String]
-- Clearly a very naive, placeholder matching function
-- 
-- Someone else probably has a nice fuzzy-matching lib we can use
genResults (c:cs) =
  foldr (\(a:as) bs -> if c == a then (a:as):bs else bs) [] database
genResults _ = database
