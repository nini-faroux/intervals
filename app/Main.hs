module Main where

import RIO
import Interval
import Options.Applicative

main :: IO ()
main = execParser options >>= \opts -> 
  runIntervals $ App (numberOfIntervals opts) (lengthIntervalOn opts) (lengthIntervalOff opts) (startSound opts) (endSound opts)
  where
    options = info (helper <*> optionsParser) fullDesc

optionsReader :: ReadM Options
optionsReader = do
  input <- str
  let [x, y, z, d, e] = splitInputByComma input
      maybeNums = readMaybe <$> [x, y, z]
   in case maybeNums of
     [Just a, Just b, Just c] -> return $ Options a b c d e
     _ -> error "Bad input"

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
    (s, acc) = foldl' (\(s, acc) x -> if x == ',' then ([], reverse s : acc) else (x:s, acc)) ([], []) xs
    ins = reverse $ reverse s : acc
