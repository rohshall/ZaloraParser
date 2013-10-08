{-# LANGUAGE OverloadedStrings #-}
import Data.List (nub)
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad (forM, sequence)
import Network.Curl.Download.Lazy (openLazyURI)
import Text.Regex.TDFA
import Text.CSV.Lazy.ByteString (toCSVTable, ppCSVTable)
import qualified Codec.Compression.GZip as GZip

-- pages of format - shop.pc.X.Y, are links of type /main_cat/sub_cat/ on the Zalora homepage
extractZaloraLinks :: L.ByteString -> [L.ByteString]
extractZaloraLinks contents =
  let links = contents =~ L.pack "href=\"(/[[:lower:]]+/[[:lower:]]+/)\"" :: [[L.ByteString]]
  in
      nub . map (!! 1) $ links


-- extract SKUs based on their format. Zalora pages have a separate id for these SKUs.
extractSKUs :: L.ByteString -> [L.ByteString]
extractSKUs contents = 
  let skus = contents =~ L.pack "id=\"([[:upper:]]{2}[[:digit:]]{3}[[:upper:]]{2}[[:digit:]]{2}[[:upper:]]{5})\"" :: [[L.ByteString]]
  in
    take 20 . nub . map (!! 1) $ skus

-- get the csv data for each link
getCsvData :: [L.ByteString] -> IO [[L.ByteString]]
getCsvData links = fmap concat $ forM links $ \link -> do
	let url = "http://www.zalora.sg" ++ L.unpack link
	    pageName = L.init $ L.append "shop.pc" $ L.intercalate "." $ L.split '/' link
	response <- openLazyURI url
	case response of
	  Left _ -> return [[L.empty]]
	  Right contents -> return $ map (\sku -> [pageName, sku]) (extractSKUs contents)

-- main
main :: IO ()
main = do
  response <- openLazyURI "http://www.zalora.sg/"
  case response of
    Left s -> error s
    Right contents -> do
      let links = extractZaloraLinks contents
      csvData <- getCsvData links
      L.writeFile "skus.csv.gz" $ GZip.compress $ ppCSVTable $ snd $ toCSVTable csvData
  
