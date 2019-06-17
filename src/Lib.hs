{-# LANGUAGE TupleSections, FlexibleInstances #-}

module Lib
    (
      dropSolution
    , solved
    , solve
    , indexed
    , create
    , render
    , Problem(..)
    , Solution(..)
    , Pos(..)
    ) where

import Data.Maybe
import Data.List
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!), (//))
import Debug.Trace
import Text.Printf

type Pos = (Int, Int)

data Problem = Problem (V.Vector Int) (V.Vector Int) [Pos]
  deriving Show

data Solution = Solution (V.Vector Int) (V.Vector Int) [Pos] [Pos]
  deriving Show


indexed :: [a] -> [(Int, a)]
indexed = zip [0..]

conflicts :: Pos -> [Pos] -> Bool
conflicts _        []               = False
conflicts t@(x1, y1) ((x2, y2):trees) = (abs (x1 - x2) <= 1 && abs (y1 - y2) <= 1) || conflicts t trees


solve (Problem oxc oyc trees) = head $ solve' oxc oyc trees []
  where
    lastIndex = V.length oxc - 1
    solve' xc yc []          solution = [ Solution oxc oyc trees (reverse solution) ]
    solve' xc yc ((x, y):ts) solution = concat [
        if x < lastIndex then test (x+1) y     else []
      , if x > 0         then test (x-1) y     else []
      , if y < lastIndex then test x     (y+1) else []
      , if y > 0         then test x     (y-1) else []
      ]
      where
        test xx yy =
          let
            nx  = pred (xc ! xx)
            ny  = pred (yc ! yy)
            in if ((xx, yy) `conflicts` solution) || nx < 0 || ny < 0
               then []
               else solve' (xc // [(xx, nx)]) (yc // [(yy, ny)]) ts ((xx, yy):solution)


solved (Solution xc yc trees tents) =
  [ V.length xc == V.length yc
  , length trees == length tents
  , checkPos trees
  , checkPos tents
  , checkTreeConnections
  , checkTentConnections tents
  , xc == count (map fst tents)
  , yc == count (map snd tents)
  ]
  where
    size = V.length xc
    checkTreeConnections =
      all (\((x1, y1), (x2, y2)) -> (x1 == x2 && abs (y1 - y2) == 1) || (y1 == y2 && abs (x1 - x2) == 1)) $ zip trees tents
    checkTentConnections [] = True
    checkTentConnections (t:ts) = not (t `conflicts` ts) && checkTentConnections ts
    checkPos = all (\(x, y) -> x >= 0 && y >= 0 && x < size && y < size)
    count l = V.accum (+) (V.replicate size 0) (map (, 1) l)

class ProblemLike a where
  dropSolution :: a -> Problem

instance ProblemLike Problem where
  dropSolution = id

instance ProblemLike Solution where
  dropSolution (Solution xc yc trees tents) = Problem xc yc trees

instance ProblemLike (Either Problem Solution) where
  dropSolution (Left that) = dropSolution that
  dropSolution (Right that) = dropSolution that

create :: V.Vector Int -> V.Vector Int -> [Pos] -> [Pos] -> Either Problem Solution
create xc yc trees []    = Left $ Problem xc yc trees
create xc yc trees tents =
  let Just (trees', tents') = paired trees tents []
  in Right $ Solution xc yc trees' tents'
  where
    paired :: [Pos] -> [Pos] -> [(Pos, Pos)] -> Maybe ([Pos], [Pos])
    paired [] [] sol = Just $ unzip sol
    paired trees tents sol = c trees tents []
      where
        c :: [Pos] -> [Pos] -> [Pos] -> Maybe ([Pos], [Pos])
        c _ [] _ = Nothing
        c xl@(x@(x1, y1):xs) (t@(x2, y2):ts) rest =
          let next = c xl ts (t : rest)
           in if (abs (x1 - x2) == 1 && y1 == y2) || (abs (y1 - y2) == 1 && x1 == x2)
                then case paired xs (ts ++ rest) $ (x, t) : sol of
                       Just sol -> Just sol
                       Nothing -> next
                else next


render :: Solution -> String
render (Solution xc yc trees tents) =
  unlines $  ("   " ++ s10) : ("   " ++ s01) : "" : rest
  where
    renderNum :: Int -> String
    renderNum = printf "% 2d "
    size = V.length yc
    ss = map renderNum $ V.toList yc
    s10 = map (!!0) ss
    s01 = map (!!1) ss
    rest = map render $ indexed $ V.toList xc
    render (rnum, sum) = renderNum sum ++ map (get rnum) [0..size-1]
    get :: Int -> Int -> Char
    get x y | (x, y) `elem` trees = 'x'
            | (x, y) `elem` tents = 't'
            | otherwise  = ' '

