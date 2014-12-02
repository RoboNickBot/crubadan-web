module Crubadan.Search ( Query, Result, Database, database, genResults ) where
                        
import Text.Regex( Regex, mkRegex, matchRegex )
import Data.Maybe( isJust )
import System.Directory (getDirectoryContents)
import qualified Data.List as L

import Crubadan.Types


database :: String -> IO Database
--database = ["abc","eng","aak","arc","src","nsa","usa","cat","sci","slu","edu"]
database path = fmap (L.delete "." . L.delete "..") (getDirectoryContents path)

genResults :: Query -> Database -> Result
genResults query = filter $ matches (mkRegex query)

matches :: Regex -> String -> Bool
matches r = isJust . matchRegex r
