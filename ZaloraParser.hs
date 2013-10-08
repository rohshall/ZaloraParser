import Network.HTTP
import Text.Regex.PCRE

extractZaloraLinks :: String -> [String]
extractZaloraLinks contents =
  let links = contents =~ "href=\"(/\\w+/\\w+)\"" :: [[String]]
  in
      ["www.zalora.sg" ++ (link !! 1) | link <- links]

main :: IO ()
main = do
  http <- simpleHTTP (getRequest "http://www.zalora.sg/") >>= getResponseBody
  let links = extractZaloraLinks http
  putStrLn $ show links
  
