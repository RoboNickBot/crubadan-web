module Crubadan.Front.Types ( Field
                            , fieldTitle
                            , fieldKey ) where

type Field = (String, String)

fieldTitle :: Field -> String
fieldTitle = fst

fieldKey :: Field -> String
fieldKey = snd
