{-# LANGUAGE BangPatterns #-}

module Main where

import Loader
import Lib
import Criterion
import Criterion.Main (defaultMain)

main :: IO ()
main = defaultMain
  [ bgroup "Solve"
    [ bench "8x8" $ nfIO $ run "8x8"
    , bench "16x16" $ nfIO $ run "16x16"
    , bench "20x20" $ nfIO $ run "20x20"
    ]
  ]
  where
    run fn = do
      d <- loadFile $ "test/" ++ fn ++ ".js"
      let !_ = solve $ dropSolution d
      return ()