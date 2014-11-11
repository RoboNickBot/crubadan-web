
import Network.CGI( CGI, CGIResult, setHeader, readInput, output, getInputs)
import Network.FastCGI( runFastCGI )
import Control.Applicative

import qualified Crubadan.Search as S

cgiMain :: CGI CGIResult
cgiMain = do setHeader "Content-type" "text/plain"
             query <- readInput "query"
             output $ show $ S.genResults <$> query <*> (pure S.database)

main :: IO ()
main = runFastCGI $ cgiMain
