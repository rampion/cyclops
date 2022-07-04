{-# LANGUAGE
    AllowAmbiguousTypes
  , BlockArguments
  , DataKinds
  , DerivingVia
  , FlexibleInstances
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

import Data.Functor.Identity (Identity(..))
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, symbolVal)
import GHC.Types (Type, Constraint, Symbol)
import System.Exit (ExitCode)

import Options.Applicative qualified as App
import Options.Applicative.Help.Chunk qualified as App

getOpsFrom :: forall m t. (Applicative m, Ops m t) => Version -> Description -> ProgName -> [String] -> m (Either OpsException t)
getOpsFrom (Version vers) (Description desc) (ProgName name) args = pure
  case
    App.execParserPure
      do App.defaultPrefs 
      do (App.info (App.helper <*> version <*> parser @m) mempty)
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

type OpsException :: Type
data OpsException
  = ExitWith ExitCode String
  | Completion (IO String)

instance Show OpsException where
  showsPrec p = \case
    ExitWith exit msg -> showParen (p > app_prec) do
      showString "ExitWith " . shows exit . showString " " . shows msg
    Completion _ -> showString "Completion (IO String)"

instance Eq OpsException where
  ExitWith exit0 msg0 == ExitWith exit1 msg1 = exit0 == exit1 && msg0 == msg1
  _ == _ = False

type Version :: Type 
newtype Version = Version String

type Description :: Type
newtype Description = Description String

type ProgName :: Type
newtype ProgName = ProgName String

type FromArgument :: (Type -> Type) -> Type -> Constraint
class FromArgument m a where
  -- m is reserved for effectual parsers in future implementations
  fromArgument :: App.ReadM a

instance FromArgument m String where
  fromArgument = App.str
  
type Ops :: (Type -> Type) -> Type -> Constraint
class Ops m t where
  -- m is reserved for effectual parsers in future implementations
  parser :: App.Parser t

instance Ops m () where
  parser = pure ()

type Arg :: Symbol -> Symbol -> Type -> Type
newtype Arg placeholder description a = Arg { arg :: a }
  deriving Eq via Identity a

instance (KnownSymbol placeholder, KnownSymbol description, Show a) => Show (Arg placeholder description a) where
  showsPrec p (Arg a) = showParen (p > app_prec) do
      showString "Arg @" .  shows (sym @placeholder) .  showString " @" . shows (sym @description) .  showString " " . showsPrec (succ app_prec) a

instance (FromArgument m a, KnownSymbol placeholder, KnownSymbol description) => Ops m (Arg placeholder description a) where
  parser = App.argument 
      do Arg <$> fromArgument @m @a
      do App.metavar (sym @placeholder) <> App.help (sym @description)

-- utilities

app_prec :: Int
app_prec = 9

sym :: forall t. KnownSymbol t => String
sym = symbolVal do Proxy @t
