module JSLoader (
  readJS
) where

import Text.ParserCombinators.Parsec
import Data.Maybe
import Lib
import qualified Data.Vector.Unboxed as V

jsParser :: GenParser Char st [(String, String)]
jsParser = do
  result <- many line
  eof
  return $ catMaybes result

line :: GenParser Char st (Maybe (String, String))
line = do
  result <- (Just <$> try varDefinition) <|> (junk >> return Nothing)
  eol
  return result


varDefinition :: GenParser Char st (String, String)
varDefinition = do
  string "var"
  sep
  name <- identifier
  sep
  string "="
  sep
  value <- quotedString '\'' <|> many digit
  sep
  string ";"
  sep
  return (name, value)

quotedString :: Char -> GenParser Char st String
quotedString qm = do
  char qm
  content <- many (quotedChar qm)
  char qm <?> "quote at end of cell"
  return content

quotedChar qm =
  noneOf [qm]
  <|> try (string ['\\', qm] >> return '"')

identifier :: GenParser Char st String
identifier = do
  x <- letter
  xs <- many alphaNum
  return $ x:xs

junk :: GenParser Char st ()
junk = skipMany (noneOf "\n")

eol :: GenParser Char st Char
eol = char '\n'

sep :: GenParser Char st ()
sep = skipMany (satisfy isSpaceOrTab)
  where
  isSpaceOrTab c = c == '\t' || c == ' '

parseJS :: String -> IO (Either ParseError [(String, String)])
parseJS = parseFromFile jsParser

readJS :: String -> IO (Either Problem Solution)
readJS fn = do
  Right vars <- parseJS fn
  let lnsize       = (read  $ get "lnsize" vars) :: Int
  let lcrownumbers = toNums $ get "lcrownumbers" vars
  let lccolnumbers = toNums $ get "lccolnumbers" vars
  let lcpuzzle     = get "lcpuzzle" vars
  let tents        = indices lnsize '2' lcpuzzle
  let trees        = indices lnsize '1' lcpuzzle
  if length lcpuzzle == lnsize * lnsize && length lcrownumbers == lnsize && length lccolnumbers == lnsize
    then return $ create (V.fromList lcrownumbers) (V.fromList lccolnumbers) trees tents
    else error "Size does not match"
  where
    get fieldName d =
      case map snd $ filter ((== fieldName) . fst) d of
        [value] -> value
        _ -> error $ "Field " ++ fieldName ++ " not found."

    toNums :: String -> [Int]
    toNums = map (read . pure)

    indices :: Int -> Char -> String -> [(Int, Int)]
    indices rows ch str = map (flip divMod rows . fst) $ filter ((==ch) . snd) $ indexed str
