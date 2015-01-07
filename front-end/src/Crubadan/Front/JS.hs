{-# LANGUAGE CPP, OverloadedStrings, ForeignFunctionInterface #-}

module Crubadan.Front.JS ( initSearchTable
                         , attachHandler
                         , readSearchTable
                         , writeResults ) where

import JavaScript.JQuery
import Data.Default (def)
import Data.Text (pack, unpack)
import Control.Event.Handler (Handler)

import Crubadan.Front.Types
import Crubadan.Shared.Types

sSearchTable = select "#searchboxtable"
sResultTable = select "#resultstable"

sSearchBox field = select (pack ("#sb" ++ (fieldKey field))) 

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
nr f = select (pack ("<td>" ++ fieldTitle f ++ "</td>"))

sr :: Field -> IO JQuery
sr f = select (pack ("<td><input id=\"sb" 
                     ++ (fieldKey f)
                     ++ "\" type=\"text\" name=\""
                     ++ (fieldTitle f)
                     ++ "\" /></td>"))

attachHandler :: [Field] -> Handler Query -> IO ()
attachHandler fields fire = foldr (watchBox fire fields) 
                                  (return ()) 
                                  fields

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

writeResults :: [Field] -> Result -> IO ()
writeResults _ rs = print rs
