{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE RecordWildCards  #-}


module Main where


import           Control.Monad
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Scientific
import           Data.Word
import           Options.Applicative


data Args = Args { argAvg :: Bool
                 , argCount :: Bool
                 , argMax :: Bool
                 , argMin :: Bool
                 , argSum :: Bool
                 , argVar :: Bool
                 , argFmt :: FPFormat
                 , argNumDec :: Int
                 } deriving (Show)

argPrintAll :: Args
            -> Args
argPrintAll a@Args{..} =
  if argAvg || argCount || argMax || argMin || argSum || argVar
  then a
  else Args True True True True True True argFmt argNumDec


parseArgs :: Parser Args
parseArgs = Args
     <$> switch
         ( long "average"
        <> short 'a'
        <> help "Print average.")
     <*> switch
         ( long "count"
        <> short 'c'
        <> help "Print record count.")
     <*> switch
         ( long "max"
        <> short 'M'
        <> help "Print max.")
     <*> switch
         ( long "min"
        <> short 'm'
        <> help "Print min.")
     <*> switch
         ( long "sum"
        <> short 's'
        <> help "Print sum.")
     <*> switch
         ( long "var"
        <> short 'v'
        <> help "Print variance.")
     <*> option parseFpFormatArg
         ( long "fp-format"
        <> short 'F'
        <> value Fixed
        <> showDefault
        <> help "Floating point format, 'e' (exponent), 'f' (fixed) or 'g' (generic)." )
     <*> option auto
         ( long "num-dec-places"
        <> short 'D'
        <> value 3
        <> showDefault
        <> help "Number of decimal places in the output." )


parseFpFormatArg :: ReadM FPFormat
parseFpFormatArg = eitherReader $ \s ->
  case s
    of "e" -> Right Exponent
       "f" -> Right Fixed
       "g" -> Right Generic
       x   -> Left $ "Failed to parse floating point format " <> x <> ", expected 'e' 'f' or 'g'"


main:: IO ()
main = execParser opts >>= runMain . argPrintAll
  where
    opts = info (helper <*> parseArgs)
      ( fullDesc
     <> progDesc ( "Reads a stream of numbers from STDIN, computes a few stats."
                <> "Singular stats are printed directly, multiple stats are prefixed with the name."
                <> "If no options are given - all stats are printed by default."
                <> "AVG and VAR are computed with Double precision, counter is 64b and SUM/MIN/MAX "
                <> "use Scientific type."
                 )
     <> header "Reads a stream of numbers from STDIN, computes a few stats." )


data StreamState = StreamState { ssCount :: !Word64
                               , ssSum :: !Scientific
                               , ssMin :: !(Maybe Scientific)
                               , ssMax :: !(Maybe Scientific)
                               , ssMean :: !Double
                               , ssM2 :: !Double -- https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance
                               } deriving (Eq, Show)

ssZero :: StreamState
ssZero = StreamState 0 0 Nothing Nothing 0 0

ssSample :: StreamState
         -> Scientific
         -> StreamState
ssSample !s !x =
  let !delta = (toRealFloat x) - (ssMean s)
      !n = (ssCount s) + 1
      !mean = (ssMean s) + delta / (fromIntegral n)
      !ssMin' = maybe x (min x) (ssMin s)
      !ssMax' = maybe x (max x) (ssMax s)
   in StreamState { ssCount = n
                  , ssSum = (ssSum s) + x
                  , ssMin = Just ssMin'
                  , ssMax = Just ssMax'
                  , ssMean = mean
                  , ssM2 = (ssM2 s) + delta*((toRealFloat x) - mean)
                  }


printStreamState :: Args
                 -> StreamState
                 -> IO ()
printStreamState Args{..} StreamState{..} = do
  let shouldPrintName = length (filter id [argAvg, argCount, argMax, argMin, argSum, argVar]) > 1
      printS fName x = putStrLn $ (if shouldPrintName then fName else "") <> (formatScientific argFmt (Just argNumDec) x)
      resAvg = toRealFloat ssSum / fromIntegral ssCount :: Double
      resVar = sqrt (ssM2 / fromIntegral (ssCount - 1)) :: Double

  when (argAvg && ssCount > 0) $ printS "AVG " $ fromFloatDigits resAvg
  when (argCount) $ printS "CNT " (fromIntegral ssCount)
  when (argMax && ssCount > 0) $ printS "MAX " (fromJust ssMax)
  when (argMin && ssCount > 0) $ printS "MIN " (fromJust ssMin)
  when (argSum) $ printS "SUM " ssSum
  when (argVar && ssCount > 1) $ printS "VAR " $ fromFloatDigits resVar


consumeInput :: String
             -> StreamState
consumeInput input = foldl' ssSample ssZero (map read (lines input))


runMain :: Args
        -> IO ()
runMain a@Args{..} = do
  ss <- getContents >>= (return . consumeInput)
  printStreamState a ss
