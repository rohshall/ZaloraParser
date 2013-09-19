import Text.HTML.TagSoup
import Network.HTTP
import Data.List
import Data.Maybe


main :: IO ()
main = do
  http <- simpleHTTP (getRequest "http://www.zalora.sg/") >>= getResponseBody
  let tags = parseTags http
      linkTags = filter (\x -> x ~== "<a href>") tags
  putStrLn $ show linkTags
  
