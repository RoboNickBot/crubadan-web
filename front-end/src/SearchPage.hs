import Reactive.Banana
import Reactive.Banana.Frameworks

import Crubadan.Front.JS
import Crubadan.Front.Types
import Crubadan.Front.Net
import Crubadan.Shared.Types

fields :: [Field]
fields = [ ("Name (English)", "name_english")
         , ("ISO Code", "lang") 
         , ("Country", "country") ]

main = do initSearchTable fields
          (addHandler, fire) <- newAddHandler
          attachHandler fields fire
          network <- compile (mkNetwork addHandler)
          actuate network

mkNetwork handler = do 
  
  eQueries <- fromAddHandler handler
  -- Just print the queries for debugging for now
  reactimate (fmap upd8 eQueries)

upd8 :: Query -> IO ()
upd8 q = print q >> netget q >>= print
