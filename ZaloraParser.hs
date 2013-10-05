import Text.HTML.TagSoup
import Network.HTTP
import Data.List
import Data.Maybe


main :: IO ()
main = do
  http <- simpleHTTP (getRequest "http://www.zalora.sg/") >>= getResponseBody
  let tags = parseTags http
      brandTags = takeWhile (~/= "</ul>") . dropWhile (~/= "<ul id=BrandsSlider>") $ tags
      links = ["www.zalora.sg/" ++ fromAttrib "href" tag | tag <- brandTags, tag ~== "<a href>"]
  putStrLn $ show links
  
