module CSVLoader (
  readCSV
) where


import Data.Maybe
import Data.List
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!), (//))
import Text.ParserCombinators.Parsec
import Data.CSV
import Debug.Trace
import Text.Printf
import Lib


readCSV :: String -> IO (Either Problem Solution)
readCSV fn = do
  Right (d:ds) <- parseFromFile csvFile fn
  let size = length ds
  let yc = V.fromList (map (read :: String -> Int) $ tail d)
  let xc = V.fromList (map ((read :: String -> Int) . head) ds)
  let trees = concatMap (row "x") $ indexed $ map tail ds
  let tents = concatMap (row "t") $ indexed $ map tail ds
  return $ create xc yc trees tents
  where
    row ch (x, r) = map (\(y, _) -> (x, y)) $ filter ((== ch) . snd) $ indexed r
