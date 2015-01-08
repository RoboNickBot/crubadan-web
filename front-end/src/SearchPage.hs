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

cgiURL url = url ++ "/cgi/" -- extension for cgi queries
wsURL url = url ++ "/ws/" -- extension for links to landing-pages

{- Fields are a three-tuple of
     
     face : the label the column has in the search table
     
     key : the database key the column corresponds to
     
     display : the function used to display field values
               (plain unless the value needs to be a
                click-able link in the table or something)
   -}
mkFields :: String -> [Field]
mkFields domain = 
  
  [ ( "Name (English)", "name_english", (withLink (wsURL u) ".html") )
  , ( "ISO Code", "lang", plain ) 
  , ( "Country", "country", plain ) ]
  
  where u = reqURL domain


{- ** END OF FRONT-END CONFIGURATION ** -}

reqURL domain = "http://" ++ domain

main = do domain <- cgiDomain
          putStrLn domain
          let fields = mkFields domain 
          initSearchTable fields
          (addHandler, fire) <- newAddHandler
          attachHandler fields fire
          network <- compile (mkNetwork domain addHandler)
          actuate network

mkNetwork domain handler = 
  do eQueries <- fromAddHandler handler 
     reactimate (fmap (upd8 domain) eQueries)

upd8 :: String -> Query -> IO ()
upd8 domain q = handleQ domain q >>= showResults (mkFields domain)

handleQ :: String -> Query -> IO (Maybe [Result])
handleQ domain q = print q >> netget (cgiURL (reqURL domain)) q

showResults :: [Field] -> Maybe [Result] -> IO ()
showResults fields rs = print rs >> writeResults fields rs
