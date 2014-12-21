import Network.CGI( CGI, CGIResult, setHeader, readInput, output, getInputs)
import Network.FastCGI( runFastCGI )
import Control.Applicative

import qualified Crubadan.Search as S
import qualified Crubadan.Types as T
import qualified Crubadan.FileIO as F

cgiMain :: T.Database -> CGI CGIResult
cgiMain d = do setHeader "Content-type" "text/plain"
               query <- readInput "query"
               output $ show $ fmap (take 20) $ S.genResults <$> query <*> (pure d)

main :: IO ()
main = do d <- F.readDatabase "./db"
          runFastCGI $ cgiMain d
          --(print . show . S.genResults "a") d
