import Crubadan.Front.JS
import Crubadan.Front.Types

fields :: [Field]
fields = [ ("Name (English)", "englishName")
         , ("Name (Native)", "nativeName")
         , ("Family", "family")
         , ("Alphabet", "alpha") ]

main = initSearchTable fields
