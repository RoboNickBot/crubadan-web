import Control.Applicative
import Reactive.Banana
import Reactive.Banana.Frameworks

import Crubadan.Front.JS
import Crubadan.Front.Types
import Crubadan.Front.Net
import Crubadan.Shared.Types


{- ** FRONT-END CONFIGURATION ** 
   
   cgiURL : the target url for back-end cgi requests
   
   wsURL : the directory for landing-page urls

   mkFields : the list of fields (in order) which are
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
  , ( "ISO Code",       "lang",         plain                        ) 
  , ( "Country",        "country",      plain                        )
  ]

  where u = reqURL domain

resultsPerPage = 50 :: Int

{- ** END OF FRONT-END CONFIGURATION ** -}


reqURL domain = "http://" ++ domain

main = do domain <- cgiDomain
          putStrLn domain
          let fields = mkFields domain 
          initSearchTable fields
          (s,p,n,r) <- getAddHandlers
          attachHandlers fields (fire s, fire p, fire n)
          network <- compile (mkNetwork domain 
                                        (fire r)
                                        ( addHandler s
                                        , addHandler p
                                        , addHandler n
                                        , addHandler r))

          -- Fill the table from empty query when page first loads
          initQuery <- readSearchTable fields
          handleRq domain (fire r) (0, resultsPerPage, initQuery)

          -- then start listening for key-ups to update
          actuate network

getAddHandlers = do searchTable <- newAddHandler
                    prevButton <- newAddHandler
                    nextButton <- newAddHandler
                    responses <- newAddHandler
                    return ( searchTable
                           , prevButton
                           , nextButton
                           , responses)

addHandler = fst
fire       = snd

mkNetwork domain sneaky (s,p,n,r) = 
  do eQueries <- fromAddHandler s
     ePrevious <- fromAddHandler p
     eNext <- fromAddHandler n
     eResponses <- fromAddHandler r 
     
     let --ePages :: Event t Int
         ePages = countPages <$> eResponses

         --bPages :: Behavior t Int
         bPages = stepper 1 ePages
         countPages (Just r) = ((responseTotal r) 
                                `div` resultsPerPage) + 1
         countPages _ = 1
         --bRangeLim :: Behavior t ((Int -> Int) -> (Int -> Int))
         bRangeLim = (\a f -> 
                        mayDo f (\x -> 
                                   (x >= 1) && (x <= a))) 
                     <$> bPages

         --eQU :: Event t (Int -> Int)
         --eQU = eQueries <$ setOne
         eQU = fmap (\_ -> setOne) eQueries
         --ePU :: Event t (Int -> Int)
         --ePU = ePrevious <$ minusOne
         ePU = fmap (\_ -> minusOne) ePrevious
         --eNU :: Event t (Int -> Int)
         --eNU = eNext <$ plusOne
         eNU = fmap (\_ -> plusOne) eNext

         --eUpdaters :: Event t (Int -> Int)
         eUpdaters = eQU `union` ePU `union` eNU

         --bCurrentPage :: Behavior t Int
         bCurrentPage = accumB 1 (bRangeLim <@> eUpdaters)

         bReqIndex = fmap (\p -> resultsPerPage * (p - 1)) bCurrentPage
         bReqNum = pure resultsPerPage
         bReqQuery = stepper [] eQueries
         bRequest = (\a b c -> (a,b,c)) 
                    <$> bReqIndex 
                    <*> bReqNum 
                    <*> bReqQuery

         sendRq = handleRq domain sneaky
     
     cRq <- changes bRequest
     reactimate' (fmap sendRq <$> (cRq))

     -- eRequests (this will come from the controls behavior) 
     -- reactimate (fmap (upd8 domain) eRequests)
     
     reactimate (fmap (showResults (mkFields domain)) eResponses)


setOne :: Int -> Int
setOne _ = 1
minusOne :: Int -> Int
minusOne a = a - 1
plusOne :: Int -> Int
plusOne a = a + 1

mayDo :: (a -> a) -> (a -> Bool) -> (a -> a)
mayDo fun test x = if test (fun x)
                      then fun x
                      else x

--upd8 :: String -> Request -> IO ()
--upd8 domain rq = handleRq domain rq >>= showResults (mkFields domain)

handleRq :: String -> Handler (Maybe Response) -> Request -> IO ()
handleRq domain fire rq = 
  print rq >> netget (cgiURL (reqURL domain)) rq >>= fire

showResults :: [Field] -> Maybe Response -> IO ()
showResults fields r = print r >> writeResponse fields r
