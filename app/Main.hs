module Main where

import Interval
import Options.Applicative

main :: IO ()
main = execParser options >>= \opts -> 
  runIntervals $ App (numberOfIntervals opts) (lengthIntervalOn opts) (lengthIntervalOff opts) (startSound opts)
  where
    options = info (helper <*> optionsParser) fullDesc

optionsReader :: ReadM Options
optionsReader = do
  input <- str
  let [x, y, z, d, e] = splitInputByComma input
      [a, b, c] = read <$> [x, y, z]
   in return $ Options a b c d e

optionsParser :: Parser Options
optionsParser = option optionsReader (short 'i')

data Options
  = Options
      { numberOfIntervals :: !Int,
        lengthIntervalOn :: !Int,
        lengthIntervalOff :: !Int,
        startSound :: !FilePath,
        endSound :: !FilePath
      } deriving Show

splitInputByComma :: String -> [String]
splitInputByComma xs = ins
  where
    (s, acc) = foldl (\(s, acc) x -> if x == ',' then ([], reverse s : acc) else (x:s, acc)) ([], []) xs
    ins = reverse $ reverse s : acc
