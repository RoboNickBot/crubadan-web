import Options.Applicative

import Crubadan.Search (genResults)
import Crubadan.FileIO (readDatabase)
import qualified Crubadan.Types as C

data Ops = Ops { dbpath :: String
               , queryStr :: String }

getOps :: Parser Ops
getOps = Ops
  <$> strOption
          ( long "path"
         <> metavar "DBPATH"
         <> help "The path to the database directory")
  <*> strOption
          ( long "search"
         <> metavar "SEARCH"
         <> help "The string to test a search with")

main = do ops <- execParser (info (helper <*> getOps)
                                   ( fullDesc
                                  <> progDesc "Search for SEARCH"
                                  <> header "web-back-test - it's a test...")) 
          db <- readDatabase (dbpath ops)
          print (genResults [("name_english", queryStr ops)] db)
          putStrLn ""
          putStrLn "-- Full Database --"
          putStrLn ""
          print db
