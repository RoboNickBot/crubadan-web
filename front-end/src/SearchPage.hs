import Reactive.Banana
import Reactive.Banana.Frameworks

import Crubadan.Front.JS
import Crubadan.Front.Types

fields :: [Field]
fields = [ ("Name (English)", "englishName")
         , ("Name (Native)", "nativeName")
         , ("Family", "family")
         , ("Alphabet", "alpha") ]

main = do initSearchTable fields
          (addHandler, fire) <- newAddHandler
          attachHandler fields fire
          network <- compile (mkNetwork addHandler)
          actuate network

mkNetwork handler = do 
  
  eQueries <- fromAddHandler handler
  -- Just print the queries for debugging for now
  reactimate (fmap (\q -> print q) eQueries)
