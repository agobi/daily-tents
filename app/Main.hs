{-# LANGUAGE BangPatterns #-}
module Main where

import Lib
import Loader
import System.Environment (getArgs)
import System.CPUTime
import Text.Printf

main' :: String -> IO ()
main' fn = do
  p <- loadFile fn
  start <- getCPUTime
  let !d = solve $ dropSolution p
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10 ^ 9)
  printf "Solved in %0.3f ms\n" (diff :: Double)
  putStrLn $ render d

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fn] -> main' fn
    _ -> putStrLn "Usage: daily-tents filename"