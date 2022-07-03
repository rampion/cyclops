{-# LANGUAGE
    BlockArguments
  , FlexibleContexts
  , LambdaCase
#-}
module Cyclops
  ( getOps
  , module Cyclops.Internal
  ) where

import Data.Function ((&))
import System.Environment (getProgName, getArgs)
import System.Exit (exitSuccess, ExitCode(ExitSuccess), exitWith)
import System.IO (hPutStrLn, stderr)

import Cyclops.Internal hiding (Ops(..))
import Cyclops.Internal (Ops())

getOps :: Ops IO t => Version -> Description -> IO t
getOps version description = do
  progName <- ProgName <$> getProgName
  getArgs >>= getOpsFrom version description progName >>= flip either pure \case
    ExitWith exit msg -> do
      msg & case exit of 
        ExitSuccess -> putStrLn
        _ -> hPutStrLn stderr
      exitWith exit
    Completion mmsg -> do
      putStr =<< mmsg
      exitSuccess
