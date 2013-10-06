import Network.HTTP
import Text.Regex.TDFA

extractZaloraLinks :: String -> [String]
extractZaloraLinks http =
  let links = getAllTextMatches $ http  =~ "href=\"/([^\"]*)" :: [String]
  in
      ["www.zalora.sg" ++ drop 6 link | link <- links]

main :: IO ()
main = do
  http <- simpleHTTP (getRequest "http://www.zalora.sg/") >>= getResponseBody
  let links = extractZaloraLinks http
  putStrLn $ show links
  
