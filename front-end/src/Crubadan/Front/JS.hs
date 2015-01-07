{-# LANGUAGE CPP, OverloadedStrings, ForeignFunctionInterface #-}

module Crubadan.Front.JS ( initSearchTable
                         , wireSearchTable
                         , readSearchTable
                         , writeResults ) where

import JavaScript.JQuery
-- import Data.Default
import Data.Text (pack)
import Control.Event.Handler (Handler)

import Crubadan.Front.Types
import Crubadan.Shared.Types

sSearchTable = select "#searchboxtable"
sResultTable = select "#resultstable"

sSearchBox name = select (pack ("#sb" ++ name)) 

initSearchTable :: [Field] -> IO ()
initSearchTable fs = do let rfs = reverse fs
                        nrow <- foldr (tablesert nr) 
                                      (select "<tr></tr>") 
                                      rfs
                        srow <- foldr (tablesert sr) 
                                      (select "<tr></tr>") 
                                      rfs
                        t <- sSearchTable 
                        appendJQuery nrow t
                        appendJQuery srow t
                        return ()

tablesert :: (Field -> IO JQuery) -> Field -> IO JQuery -> IO JQuery
tablesert fun field iojq = do child <- fun field
                              parent <- iojq
                              appendJQuery child parent

nr :: Field -> IO JQuery
nr (s,_) = select (pack ("<td>" ++ s ++ "</td>"))

sr :: Field -> IO JQuery
sr (s,r) = select (pack ("<td><input id=\"sb" 
                         ++ r 
                         ++ "\" type=\"text\" name=\""
                         ++ s
                         ++ "\" /></td>"))

wireSearchTable :: [Field] -> Handler a -> IO ()
wireSearchTable = undefined

readSearchTable :: [Field] -> IO Query
readSearchTable = undefined

writeResults :: [Field] -> Result -> IO ()
writeResults = undefined
