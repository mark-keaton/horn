module Main where

import           Data.Semigroup      ((<>))
import           Options.Applicative

import           Text.Pretty.Simple  (pPrint)

import           Plan

data Options = Options
  { day        :: Integer }

getOptions :: Parser Options
getOptions = Options
      <$> option auto
          ( long "day"
         <> short 'd'
         <> metavar "INT"
         <> help "Day to produce reading plan for, e.g., 1" )

main :: IO ()
main = printPlan =<< execParser opts
  where
    opts = info (getOptions <**> helper)
      ( fullDesc
     <> progDesc "Calculate readings for day"
     <> header "horner - a Bible reading plan assistant for the Barry Horner plan" )

printPlan :: Options -> IO()
printPlan (Options day) = pPrint $ getReadingForDay day

