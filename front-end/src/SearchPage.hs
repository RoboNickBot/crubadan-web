import Reactive.Banana
import Reactive.Banana.Frameworks

import Crubadan.Front.JS
import Crubadan.Front.Types
import Crubadan.Front.Net
import Crubadan.Shared.Types


{- ** FRONT-END CONFIGURATION ** 
   
   domain : the target url for back-end requests

   fields : the list of fields (in order) which are
            to be displayed for the user in the
            search table
   -}
domain :: String
domain = "http://localhost"

cgiURL = domain ++ "/cgi/" -- extension for cgi queries
wsURL = domain ++ "/ws/" -- extension for links to landing-pages

{- Fields are a three-tuple of
     
     face : the label the column has in the search table
     
     key : the database key the column corresponds to
     
     display : the function used to display field values
               (plain unless the value needs to be a
                click-able link in the table or something)
   -}
fields :: [Field]
fields = 
  [ ( "Name (English)", "name_english", (withLink wsURL ".html") )
  , ( "ISO Code", "lang", plain ) 
  , ( "Country", "country", plain ) ]


{- ** END OF FRONT-END CONFIGURATION ** -}


main = do initSearchTable fields
          (addHandler, fire) <- newAddHandler
          attachHandler fields fire
          network <- compile (mkNetwork addHandler)
          actuate network

mkNetwork handler = 
  do eQueries <- fromAddHandler handler 
     reactimate (fmap upd8 eQueries)

upd8 :: Query -> IO ()
upd8 q = handleQ q >>= showResults

handleQ :: Query -> IO (Maybe [Result])
handleQ q = print q >> netget cgiURL q

showResults :: Maybe [Result] -> IO ()
showResults rs = print rs >> writeResults fields rs
