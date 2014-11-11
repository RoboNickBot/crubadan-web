{-# LANGUAGE CPP, OverloadedStrings, ForeignFunctionInterface #-}

import Data.Default
import qualified Data.Text as T (empty, pack, unpack, Text)

import JavaScript.Canvas
import JavaScript.JQuery hiding (Event)
import GHCJS.Types
import GHCJS.Foreign

import Reactive.Banana
import Reactive.Banana.Frameworks

import Data.Maybe

import qualified Crubadan.Types as S (Database)
import qualified Crubadan.Net as N (netget)

main = do
  searchBox <- initSearchBox
  resultsBox <- initResultsBox
  tEvent <- wireSearchBox searchBox
  network <- compile $ netDesc tEvent resultsBox
  actuate network

initSearchBox :: IO JQuery
initSearchBox = do
  parent <- select "#searchboxdiv"
  box <- select "<input id=\"searchbox\" type=\"text\" name=\"search\" />"
  appendJQuery box parent
  searchbox <- select "#searchbox"
  return searchbox

initResultsBox :: IO JQuery
initResultsBox = select "#resultsdiv"

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
    r :: JQuery -> S.Database -> IO ()
    r parent (s:ss) = do
      p <- select "<p>"
      setText (T.pack s) p
      appendJQuery p parent
      r parent ss
    r _ _ = return ()
  in
    do
      --print $ "testing......"
      zs <- N.netget s
      --print $ "testing..."
      --print $ show zs
      JavaScript.JQuery.empty i
      r i $ fromJust zs 
