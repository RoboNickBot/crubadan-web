module Crubadan.FileIO ( readDatabase ) where

import System.Directory (getDirectoryContents)
import qualified Text.ParserCombinators.Parsec as P
import qualified Data.Map as M
import qualified Data.List as L

import qualified Crubadan.Types as T

recordFilename = "EOLAS"

readDatabase :: String -> IO T.Database
readDatabase = fmap processEntries . entries

entries :: String -> IO [String]
entries path = getNames path >>= foldr entryAppend (return [])

getNames :: String -> IO [String]
getNames path = ( fmap (fmap (qualifyName path))
                . fmap (L.delete "." . L.delete "..") 
                . getDirectoryContents
                ) path

qualifyName :: String -> String -> String
qualifyName path name = path ++ "/" ++ name ++ "/" ++ recordFilename

entryAppend :: String -> IO [String] -> IO [String]
entryAppend name ss = 
  do content <- readFile name
     others <- ss
     return (content : others)

processEntries :: [String] -> T.Database
processEntries = foldr p []
  where p f ss = case P.parse wsFile "(unknown)" f of
                   Left _ -> ss
                   Right w -> w : ss

wsFile :: P.GenParser Char st T.WS
wsFile = do name <- wsName
            P.char '\n'
            rs <- wsRecords
            return (T.WS name (M.fromList rs))

wsName = P.string "lang " >> P.many (P.noneOf "\n")

wsRecords = do record <- wsRecord
               more <- moreRecords
               return ( record : more )

wsRecord = do key <- P.many (P.noneOf " ")
              P.char ' '
              val <- P.many (P.noneOf "\n")
              return ( key, T.Attribute val val )

moreRecords = (P.char '\n' >> wsRecords) P.<|> (return [])
