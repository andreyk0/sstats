{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE RecordWildCards  #-}


module Main where


import           Args
import           Control.Monad
import           Data.Conduit
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Scientific
import           Data.Word
import           System.IO


main:: IO ()
main = runWithArgs runMain


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
  hSetBinaryMode stdin True
  hSetBuffering stdin (BlockBuffering Nothing)

  ss <- getContents >>= (return . consumeInput)
  printStreamState a ss
