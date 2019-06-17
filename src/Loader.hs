module Loader where

import Lib
import Data.List
import JSLoader
import CSVLoader

loadFile :: String -> IO (Either Problem Solution)
loadFile fileName =
  case fileName of
    _ | ".csv" `isSuffixOf` fileName -> readCSV fileName
    _ | ".js" `isSuffixOf` fileName -> readJS fileName
    _ -> error "Unsupported file format"
