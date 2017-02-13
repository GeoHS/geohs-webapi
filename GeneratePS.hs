module GeneratePS (main) where

import           GeoHS.WebAPI.GeneratePS (generatePS)
import           System.Environment (getArgs)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [outputDir] -> generatePS outputDir
    _ -> putStrLn "Usage: geohs-purecsript <outputDir>"
