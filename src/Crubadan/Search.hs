module Crubadan.Search ( Query, Result, Database, database, genResults ) where
                        
import Text.Regex( Regex, mkRegex, matchRegex )
import Data.Maybe( isJust )
import Crubadan.Types


database :: Database
database = ["abc","eng","aak","arc","src","nsa","usa","cat","sci","slu","edu"]

genResults :: Query -> Database -> Result
genResults query = filter $ matches (mkRegex query)

matches :: Regex -> String -> Bool
matches r = isJust . matchRegex r
