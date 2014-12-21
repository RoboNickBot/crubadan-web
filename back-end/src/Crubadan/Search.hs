module Crubadan.Search ( genResults ) where

import Text.Regex( Regex, mkRegex, matchRegex )
import Data.Maybe( isJust )
import System.Directory (getDirectoryContents)
import qualified Data.List as L

import qualified Crubadan.Types as T

genResults :: T.Query -> T.Database -> T.Result
genResults query = undefined --filter $ matches (mkRegex query)

matches :: Regex -> String -> Bool
matches r = isJust . matchRegex r
