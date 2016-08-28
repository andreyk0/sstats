{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE RecordWildCards  #-}


module Args (
  Args(..)
, runWithArgs
) where


import           Data.Monoid
import           Data.Scientific
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


runWithArgs:: (Args -> IO ())
           -> IO ()
runWithArgs rwa = execParser opts >>= rwa . argPrintAll
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
