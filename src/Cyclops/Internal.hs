{-# LANGUAGE
    AllowAmbiguousTypes
  , BlockArguments
  , ImportQualifiedPost
  , LambdaCase
  , MultiParamTypeClasses
  , Rank2Types
  , ScopedTypeVariables
  , StandaloneKindSignatures
  , TypeApplications
  , ViewPatterns
#-}
module Cyclops.Internal where

import Control.Applicative ((<**>))
import GHC.Types (Type, Constraint)
import System.Exit (ExitCode)

import Options.Applicative qualified as App
import Options.Applicative.Help.Chunk qualified as App

getOpsFrom :: forall m t. (Applicative m, Ops m t) => Version -> Description -> ProgName -> [String] -> m (Either OpsException t)
getOpsFrom (Version vers) (Description desc) (ProgName name) args = pure
  case
    App.execParserPure
      do App.defaultPrefs 
      do (App.info (parser @m <**> App.helper <**> version) mempty)
          { App.infoFooter = App.stringChunk desc
          }
      do args
  of
    App.Success t -> Right t
    App.Failure ((`App.renderFailure` name) -> (msg, exit)) -> Left do ExitWith exit msg
    App.CompletionInvoked ((`App.execCompletion` name) -> cmpl) -> Left do Completion cmpl
  where
    version = App.infoOption
      do unwords [name, vers]
      do mconcat 
          [ App.short 'v'
          , App.long "version"
          , App.help do "Show the version (" ++ vers ++ ") and exit"
          ]
  
type Ops :: (Type -> Type) -> Type -> Constraint
class Ops m t where
  -- m is reserved for effectual parsers in future implementations
  parser :: App.Parser t

type OpsException :: Type
data OpsException
  = ExitWith ExitCode String
  | Completion (IO String)

type Version :: Type 
newtype Version = Version String

type Description :: Type
newtype Description = Description String

type ProgName :: Type
newtype ProgName = ProgName String
