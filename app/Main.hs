module Main (main) where

import Data.List.Split (splitOn)
import Interpolation (lagrangeList)
import Options.Applicative
import System.IO
import Text.Read (readMaybe)

---- options and parsing ----

data Options = Options
  { step :: Double,
    window :: Int,
    method :: [String]
  }

stepOption :: Parser Double
stepOption =
  option
    auto
    ( long "step"
        <> help "Step value"
        <> metavar "DOUBLE"
    )

windowOption :: Parser Int
windowOption =
  option
    auto
    ( long "window"
        <> help "window size"
        <> metavar "INTEGER"
    )

methodOption :: Parser [String]
methodOption =
  some $
    strOption
      ( long "method"
          <> help "Method name (e.g. 'lagrange')"
          <> metavar "[STRING]"
      )

optionsParser :: Parser Options
optionsParser =
  Options
    <$> stepOption
    <*> windowOption
    <*> methodOption

fromMaybe :: Maybe a -> a
fromMaybe Nothing = error "nothing to unpack"
fromMaybe (Just x) = x

toPair :: String -> String -> (Double, Double)
toPair sep fromStr = (head l, l !! 1)
  where
    l = map (fromMaybe . stringToDouble) $ splitOn sep fromStr

stringToDouble :: String -> Maybe Double
stringToDouble = readMaybe

maybeReadPoint :: IO (Maybe (Double, Double))
maybeReadPoint = do
  eof <- isEOF
  if eof
    then return Nothing
    else do
      Just . toPair ";" <$> getLine

---- main routine ----

main :: IO ()
main = execParser opts >>= runWithOptions
  where
    opts =
      info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "Linear interpolation. Functional programming 2023."
            <> header "Linear interpolation"
        )

runWithOptions :: Options -> IO ()
runWithOptions (Options mStep mWindow mMethod) = do
  putStrLn $ "Step: " ++ show mStep
  putStrLn $ "Window: " ++ show mWindow
  putStrLn $ "Method: " ++ show mMethod
  cliRoutine mStep mWindow [] True

cliRoutine :: Double -> Int -> [(Double, Double)] -> Bool -> IO ()
cliRoutine step' window' vals instart = do
  maybePoint <- maybeReadPoint
  case maybePoint of
    Just pt -> start $ vals ++ [pt]
    Nothing -> do
      interpolateInInterval lagrangeList vals (fst $ head vals) (fst $ last vals) step'
  where
    start :: [(Double, Double)] -> IO ()
    start vals'
      | length vals' < window' = cliRoutine step' window' vals True
      | otherwise = do
          let middleX = (fst (head vals') + fst (last vals')) / 2
          if instart
            then do
              interpolateInInterval lagrangeList vals' (fst $ head vals') (fst $ last vals') step'
            else do
              interpolateInInterval lagrangeList vals' middleX middleX 1
          cliRoutine step' window' (drop 1 vals') False

interpolateInInterval :: ([Double] -> [(Double, Double)] -> [(Double, Double)]) -> [(Double, Double)] -> Double -> Double -> Double -> IO ()
interpolateInInterval interpolateFunc vals fromX toX freq = do
  let pts = [fromX, (fromX + freq) .. toX]
  mapM_ print (interpolateFunc pts vals)
