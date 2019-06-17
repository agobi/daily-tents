{-# LANGUAGE TupleSections #-}

module Main where

import Lib
import Loader
import Test.Hspec
import Control.Exception (evaluate)
import Control.Monad
import Data.Either
import Data.List

testFiles = ["solution1.csv", "20x20.csv", "8x8.js", "16x16.js", "20x20.js"]

loadFile' :: String -> IO (String, Either Problem Solution)
loadFile' fileName = (fileName, ) <$> loadFile ("test/" ++ fileName)

onSnd :: (a -> b) -> (c, a) -> (c, b)
onSnd f (c, a) = (c, f a)

main :: IO ()
main = do
  tests <- mapM loadFile' testFiles
  let problems = map (onSnd dropSolution) tests
  let solutions = rights $ map sequenceA tests

  hspec $ do
    describe "checker" $
      forM_ solutions $ \(file, solution) ->
        it ("should check " ++ file) $
          and (solved solution) `shouldBe` True
    describe "solver" $
      forM_ problems $ \(file, problem) ->
        it ("should solve " ++ file) $
          let solution = solve $ dropSolution problem
              x  = solved solution::[Bool]
          in and x `shouldBe` True
