{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Crubadan.Front.JS ( initSearchTable
                         , attachHandlers -- TODO
                         , readSearchTable
                         , writeResponse ) where

import JavaScript.JQuery
import Data.Default (def)
import Data.Text (pack, unpack)
import Control.Event.Handler (Handler)

import Crubadan.Front.Types
import Crubadan.Shared.Types

searchConID = "searchcontrols" :: String
searchTableID = "searchtable" :: String
resultRowClass = "resultrow" :: String
indexInfoName = "indexinfo" :: String
prevButtonName = "prevbutton" :: String
prevButtonLabel = "<- Previous"
nextButtonName = "nextbutton" :: String
nextButtonLabel = "Next ->"

sPrevButton = selp ("#" ++ prevButtonName)
sNextButton = selp ("#" ++ nextButtonName)
sPrevButton' = 
  selp ("<button id=\"" ++ prevButtonName
        ++ "\">" ++ prevButtonLabel ++ "</button>")
sNextButton' = 
  selp ("<button id=\"" ++ nextButtonName
        ++ "\">" ++ nextButtonLabel ++ "</button>")

sResultRows = selp ("." ++ resultRowClass)
sSearchTable = selp ("#" ++ searchTableID)

sSearchTable' = selp ("<table id=\"" 
                      ++ searchTableID
                      ++ "\"></table>")

sSearchConDiv = selp ("#" ++ searchConID ++ "div")
sSearchTableDiv = selp ("#" ++ searchTableID ++ "div")

sSearchBox field = selp ("#sb" ++ (fieldKey field)) 

sIndexInfoDiv = selp ("#" ++ indexInfoName ++ "div")

sIndexInfo = selp ("#" ++ indexInfoName)
sIndexInfo' :: (Int, Int, Int, Int) -> IO JQuery 
sIndexInfo' (start, end, num, total) = 
  selp ("<span id=\"" ++ indexInfoName ++ "\">Showing "
        ++ show start ++ " to " ++ show end 
        ++ " of " ++ show num ++ totalClause
        ++ "</span>")
  where totalClause = if num == total
                         then " entries"
                         else " results (filtered from "
                              ++ show total
                              ++ " total entries)"

selp = select . pack

initSearchTable :: [Field] -> IO ()
initSearchTable fs = do initControls
                        table <- sSearchTable'
                        tdiv <- sSearchTableDiv
                        appendJQuery table tdiv
                        let rfs = reverse fs
                        nrow <- foldr (tablesert nr) 
                                      (selp "<tr></tr>") 
                                      rfs
                        srow <- foldr (tablesert sr) 
                                      (selp "<tr></tr>") 
                                      rfs 
                        appendJQuery nrow table
                        appendJQuery srow table
                        return ()

initControls :: IO ()
initControls = do d <- sSearchConDiv
                  info <- sIndexInfo' (0,0,0,0)
                  pb <- sPrevButton'
                  nb <- sNextButton'
                  appendJQuery info d
                  appendJQuery pb d
                  appendJQuery nb d
                  return ()

tablesert :: (Field -> IO JQuery) -> Field -> IO JQuery -> IO JQuery
tablesert fun field iojq = do child <- fun field
                              parent <- iojq
                              appendJQuery child parent

nr :: Field -> IO JQuery
nr f = selp ("<td>" ++ fieldTitle f ++ "</td>")

sr :: Field -> IO JQuery
sr f = selp ("<td><input id=\"sb" 
             ++ (fieldKey f)
             ++ "\" type=\"text\" name=\""
             ++ (fieldTitle f)
             ++ "\" /></td>")

attachHandlers :: [Field] 
               -> ( Handler Query
                  , Handler ()
                  , Handler () ) 
               -> IO ()
attachHandlers fields (s,p,n) = foldr (watchBox s fields) 
                                      (return ()) 
                                      fields
                                >> sPrevButton >>= watchButton p
                                >> sNextButton >>= watchButton n

watchButton :: Handler () -> JQuery -> IO ()
watchButton fire button = let h _ = return () >>= fire
                          in click h def button >> return ()

watchBox :: Handler Query -> [Field] -> Field -> IO () -> IO ()
watchBox fire fs field io = 
  io >> do let handler _ = fire =<< readSearchTable fs
           box <- sSearchBox field
           keyup handler def box
           return ()

readSearchTable :: [Field] -> IO Query
readSearchTable = foldr quappend (return [])

quappend :: Field -> IO Query -> IO Query
quappend f ioq = do q <- ioq
                    val <- readField f
                    return ((fieldKey f, val) : q)

readField :: Field -> IO String
readField f = sSearchBox f >>= fmap unpack . getVal

writeResponse :: [Field] -> Maybe Response -> IO ()
writeResponse fs (Just r) = 
  do oldinfo <- sIndexInfo
     remove oldinfo
     let ds = ( (responseIndex r + 1), (responseIndex r + responseNum r)
              , (responseTotal r), (responseAll r) )
     newinfo <- sIndexInfo' ds
     par <- sIndexInfoDiv
     appendJQuery newinfo par
     writeResults fs (Just (responseData r))
writeResponse fs Nothing = 
  sIndexInfo >>= remove >> writeResults fs Nothing 

writeResults :: [Field] -> Maybe [Result] -> IO ()
writeResults fs (Just rs) = do clearTable
                               table <- sSearchTable
                               foldr (rowsert table fs) (return ()) rs 
writeResults _ _ = putStrLn "Connection or Database Error!"

clearTable :: IO ()
clearTable = sResultRows >>= remove >> return ()

rowsert :: JQuery -> [Field] -> Result -> IO () -> IO ()
rowsert table fs r io = 
  io >> do row <- selp ("<tr class=\"" 
                        ++ resultRowClass
                        ++ "\"></tr>")
           appendJQuery row table
           foldr (colsert row r) 
                 (return ()) 
                 (reverse (zip fs (resultData r)))
           return ()

colsert :: JQuery -> Result -> (Field, Maybe String) -> IO () -> IO ()
colsert row r (f, d) io = 
  io >> do let disp = fieldDisplay f
           td <- selp ("<td>" ++ disp r d ++ "</td>")
           appendJQuery td row
           return ()
