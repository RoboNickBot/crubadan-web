import Network.CGI( CGI, CGIResult, setHeader, readInput, output, getInputs)
import Network.FastCGI( runFastCGI )
import Control.Applicative

import qualified Crubadan.Search as S
import qualified Crubadan.Types as C
import qualified Crubadan.FileIO as F

cgiMain :: C.Database -> CGI CGIResult
cgiMain d = 
  do setHeader "Content-type" "text/plain"
     request <- readInput "query"
     output $ show $ S.genResponse <$> request <*> (pure d)

main :: IO ()
main = do d <- F.readDatabase "/data/crubadan/key-values"
          runFastCGI $ cgiMain d
          --(print . show . S.genResults "a") d
