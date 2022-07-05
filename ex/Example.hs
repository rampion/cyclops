{-# LANGUAGE
    BlockArguments
  , DataKinds
  , OverloadedStrings
  , TypeApplications
  , ViewPatterns
#-}
module Main where

import Cyclops
import Control.Monad (when, replicateM_)
import Data.List.NonEmpty (toList)

main :: IO ()
main = do
  ( switch @'["d", "debug"] 
           @"Show debugging statements"
    -> debug

   ,flagged @'["dictionary"] 
            @"PATH" 
            @"File to draw words from"
            .defaultTo @"--dictionary /usr/share/dict/words"
    -> dictionaryPath

   ,option @'["s", "seed"]
           @"NUM"
           @"Seed value for PRNG" 
           .readable
    -> seed

   ,arg @"count"
        @"Number of passwords to generate"
        .readable
    -> count

   ,arg @"template"
        @"Template for generated passwords"
        .oneOrMore
        .defaultTo @"w w w w"
    -> template

   ) <- getOps Conf
      { description = "Generate pseudorandom passwords using a template." 
      , version = "1.0.0.20220702"
      }

  when debug do
    putStrLn "debugging enabled"

  dict <- lines <$> readFile dictionaryPath

  case seed of
    Nothing -> mempty
    Just n  -> setStdGen n

  replicateM_ count do
    putStrLn . unwords . toList =<< traverse (generate dict) template 

setStdGen :: Int -> IO ()
setStdGen = putStrLn . ("setStdGen " ++) . show

generate :: [String] -> String -> IO String
generate _ w = pure do "«" ++ w ++ "»"
