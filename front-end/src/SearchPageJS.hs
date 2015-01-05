{-# LANGUAGE CPP, OverloadedStrings, ForeignFunctionInterface #-}

import Data.Default
import qualified Data.Text as T

import JavaScript.JQuery hiding (Event)

import Reactive.Banana
import Reactive.Banana.Frameworks

import Data.Maybe

import qualified Crubadan.Shared.Types as CS
import qualified Crubadan.Net as N

main = do
  searchBox <- initSearchBox
  resultsBox <- initResultsBox
  tEvent <- wireSearchBox searchBox
  network <- compile $ netDesc tEvent resultsBox
  actuate network

initSearchBox :: IO JQuery
initSearchBox = do
  parent <- select "#searchboxtable"
  box <- select "<tr><td><input id=\"searchbox\" type=\"text\" name=\"search\" /></td></tr>"
  appendJQuery box parent
  searchbox <- select "#searchbox"
  return searchbox

initResultsBox :: IO JQuery
initResultsBox = select "#resultstable"

netDesc :: Frameworks t
        => AddHandler (String)
        -> JQuery
        -> Moment t ()
netDesc addSearchEvent results = do
  searches <- fromAddHandler addSearchEvent
  let bSearches = stepper [] searches
  searchChanges <- changes bSearches
  reactimate' $ fmap (update results) <$> searchChanges

wireSearchBox :: JQuery -> IO (AddHandler String)
wireSearchBox box = do
  (addHandler, fire) <- newAddHandler
  --getVal box
  let handler _ = fire =<< getValString box
  keyup handler def box
  return addHandler

getValString :: JQuery -> IO String
getValString box = do
  t <- getVal box
  return $ T.unpack t

update :: JQuery -> String -> IO ()
update i s =
  let
    ln name uid = "<td><a href=\"http://octalsrc.net/db/" ++ uid ++ "\">" ++ name ++ "</a></td>"
    r :: JQuery -> CS.Result -> IO ()
    r parent (s:ss) = do
      p <- select $ T.pack ("<tr>" ++ (ln (head . snd $ s) (fst s)) ++ "<\tr>")
      --setText (T.pack (ln s)) p
      appendJQuery p parent
      r parent ss
    r _ _ = return ()
  in
    do
      --print $ "testing......"
      zs <- N.netget [("name_english",s)]
      --print $ "testing..."
      --print $ show zs
      JavaScript.JQuery.empty i
      r i $ fromJust zs 
