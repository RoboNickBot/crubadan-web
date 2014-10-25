import Network.CGI
import Network.FastCGI
import Text.XHtml

valids :: [String]
valids = ["abc", "eng", "zzz"]

errorPage :: String -> Html
errorPage name = body << h1 << ("404: Language \"" ++ name ++ "\" is not in the database.")

page :: String -> Html
page t = 
  let title = "Language Code: [" ++ t ++ "]"
      status = "Status: Resources Available"
      desc  = "Information on the language \"" ++ t ++ "\", including download links for the resource zip files, would be on this page, laid out in a table or list."
  in body << ((h1 << title) +++ (p << status) +++ (p << desc))

endof :: String -> String
endof = snd . foldr (\c (b,s) -> if c /= '/' && b then (b,c:s) else (False,s)) (True,[])

checkError :: Maybe String -> String
checkError (Just ss) = ss
checkError _ = "error"

cgiMain :: CGI CGIResult
cgiMain = do url <- getVar "DOCUMENT_URI"
             let name = checkError $ fmap endof url
             if name `elem` valids
             then output . renderHtml . page $ name
             else setStatus 404 "Page not found" >>= (\a -> output . renderHtml $ errorPage name)

main :: IO ()
main = runFastCGI $ cgiMain
