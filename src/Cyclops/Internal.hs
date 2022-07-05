{-# LANGUAGE
    AllowAmbiguousTypes
  , BlockArguments
  , DataKinds
  , DerivingVia
  , FlexibleInstances
  , FunctionalDependencies
  , ImportQualifiedPost
  , LambdaCase
  , KindSignatures
  , MultiParamTypeClasses
  , Rank2Types
  , ScopedTypeVariables
  , StandaloneKindSignatures
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
  , ViewPatterns
#-}
module Cyclops.Internal where

import Control.Applicative ((<|>), liftA2)
import Data.Functor.Classes (Eq1)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (Identity(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
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
      do (App.info (helper <*> version <*> parser @m) mempty)
          { App.infoFooter = App.stringChunk desc
          }
      do args
  of
    App.Success t -> Right t
    App.Failure ((`App.renderFailure` name) -> (msg, exit)) -> Left do ExitWith exit msg
    App.CompletionInvoked ((`App.execCompletion` name) -> cmpl) -> Left do Completion cmpl
  where
    helper = App.helper
    version = App.infoOption
      do unwords [name, vers]
      do mconcat 
          [ App.short 'v'
          , App.long "version"
          , App.help do "Show the version (" ++ vers ++ ") and exit"
          ]

-- utilities

app_prec :: Int
app_prec = 9

sym :: forall t. KnownSymbol t => String
sym = symbolVal do Proxy @t

syms :: forall ts. KnownSymbols ts => [String]
syms = symbolVals do Proxy @ts

type KnownSymbols :: [Symbol] -> Constraint
class KnownSymbols ns where
  symbolVals :: Proxy ns -> [String]

instance KnownSymbols '[] where
  symbolVals _ = []

instance (KnownSymbols ns, KnownSymbol n) => KnownSymbols (n ': ns) where
  symbolVals _ = sym @n : syms @ns
-- /utilities

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

newtype ReadArgument a = ReadArgument { getReadArgument :: a }

instance Read a => FromArgument m (ReadArgument a) where
  fromArgument = ReadArgument <$> App.auto
  
type Ops :: (Type -> Type) -> Type -> Constraint
class Ops m t where
  -- m is reserved for effectual parsers in future implementations
  parser :: App.Parser t

instance Ops m () where
  parser = pure ()

type Arg :: Symbol -> Symbol -> Type -> Type
newtype Arg placeholder description a = Arg { arg :: a }
  deriving Eq via Identity a
  deriving (Eq1, Functor, Applicative) via Identity

instance (KnownSymbol placeholder, KnownSymbol description, Show a) => Show (Arg placeholder description a) where
  showsPrec p (Arg a) = showParen (p > app_prec) do
      showString "Arg @" .  shows (sym @placeholder) .  showString " @" . shows (sym @description) .  showString " " . showsPrec (succ app_prec) a

instance (FromArgument m a, KnownSymbol placeholder, KnownSymbol description) => Ops m (Arg placeholder description a) where
  parser = App.argument 
      do Arg <$> fromArgument @m @a
      do App.metavar (sym @placeholder) <> App.help (sym @description)

type Readable :: (Type -> Type) -> Type -> Type
newtype Readable f a = Readable { readable :: f a }
  deriving (Functor, Applicative) via f

instance Show (f a) => Show (Readable f a) where
  showsPrec p (Readable fa) = showParen (p > app_prec) do
    showString "Readable " . showsPrec (succ app_prec) fa

instance (Functor f, Ops m (f (ReadArgument a))) => Ops m (Readable f a) where
  parser = Readable . fmap getReadArgument <$> parser @m

type Optional :: (Type -> Type) -> Type -> Type
newtype Optional f a = Optional { optional :: f (Maybe a) }
  deriving Eq via Compose f Maybe a

instance Show (f (Maybe a)) => Show (Optional f a) where
  showsPrec p (Optional fa) = showParen (p > app_prec) do
    showString "Optional " . showsPrec (succ app_prec) fa

instance (Applicative f, Ops m (f a)) => Ops m (Optional f a) where
  parser = (Optional . fmap Just <$> parser @m) <|> pure do Optional do pure Nothing

type DefaultTo :: Symbol -> (Type -> Type) -> Type -> Type
newtype DefaultTo defaultValue f a = DefaultTo { defaultTo :: f a }
  deriving Eq via f a

instance (KnownSymbol defaultValue, Show (f a)) => Show (DefaultTo defaultValue f a) where
  showsPrec p (DefaultTo fa) = showParen (p > app_prec) do
    showString "DefaultTo @" . shows (sym @defaultValue) . showString " " . showsPrec (succ app_prec) fa

instance (Applicative f, Ops m (f a), KnownSymbol defaultValue) => Ops m (DefaultTo defaultValue f a) where
  parser = primary <|> fallback where
    defaultValue = sym @defaultValue
    primary = DefaultTo <$> parser @m
    fallback = case
        App.execParserPure
          do App.defaultPrefs 
          do App.info primary mempty
          do words defaultValue
      of
        App.Success t -> pure t
        _ -> error do "Unparseable default value " ++ show defaultValue

type ZeroOrMore :: (Type -> Type) -> Type -> Type
newtype ZeroOrMore f a = ZeroOrMore { zeroOrMore :: f [a] }
  deriving Eq via Compose f [] a

instance Show (f [a]) => Show (ZeroOrMore f a) where
  showsPrec p (ZeroOrMore fla) = showParen (p > app_prec) do
    showString "ZeroOrMore " . showsPrec (succ app_prec) fla

instance (Applicative f, Ops m (f a)) => Ops m (ZeroOrMore f a) where
  parser = ZeroOrMore . sequenceA <$> App.many (parser @m)

type OneOrMore :: (Type -> Type) -> Type -> Type
newtype OneOrMore f a = OneOrMore { oneOrMore :: f (NonEmpty a) }
  deriving Eq via Compose f NonEmpty a

instance Show (f (NonEmpty a)) => Show (OneOrMore f a) where
  showsPrec p (OneOrMore fla) = showParen (p > app_prec) do
    showString "OneOrMore " . showsPrec (succ app_prec) fla

instance (Applicative f, Ops m (f a)) => Ops m (OneOrMore f a) where
  parser = OneOrMore . sequenceA <$> liftA2 (:|) p (App.many p)
    where p = parser @m

type Flag :: [Symbol] -> Symbol -> Type -> Type
newtype Flag flags description a = Flag a
  deriving (Functor, Applicative) via Identity

type Set :: Type -> Type -> Type -> Constraint
class Set a s t | a s -> t, s t -> a where
  set :: a -> s -> t

instance Set a () a where
  set = const

instance Functor f => Set a (f b) (f a) where
  set = (<$)

flag ::
  forall (flags :: [Symbol]) (description :: Symbol) (a :: Type) (s :: Type) (t :: Type).
  Set a s t => a -> Flag flags description s -> t
flag a (Flag s) = set a s

instance (KnownSymbols flags, KnownSymbol description, Show a) => Show (Flag flags description a) where
  showsPrec p (Flag a) = showParen (p > app_prec) do
    showString "Flag @" . shows (syms @flags) . showString " @" . shows (sym @description) . showString " " . shows a

instance (KnownSymbols flags, KnownSymbol description, a ~ ()) => Ops m (Flag flags description a) where
  parser = App.flag' 
      do Flag ()
      do foldMap (go . dropWhile (=='-')) (syms @flags) <> App.help (sym @description)
    where go [] = error "invalid flag"
          go [c] = App.short c
          go w = App.long w

type Flagged :: [Symbol] -> Symbol -> Symbol -> Type -> Type
newtype Flagged flags placeholder description a = Flagged { flagged :: a }
  deriving (Functor, Applicative) via Identity

instance (KnownSymbols flags, KnownSymbol placeholder, KnownSymbol description, Show a) => Show (Flagged flags placeholder description a) where
  showsPrec p (Flagged a) = showParen (p > app_prec)
    do showString "Flagged @" . shows (syms @flags) .
        showString " @" . shows (sym @placeholder) .
        showString " @" . shows (sym @description) .
        showString " " . shows a

instance (KnownSymbols flags, KnownSymbol placeholder, KnownSymbol description, FromArgument m a) => Ops m (Flagged flags placeholder description a) where
  parser = App.option
      do Flagged <$> fromArgument @m
      do foldMap (go . dropWhile (=='-')) (syms @flags) <> App.help (sym @description) <> App.metavar (sym @placeholder)
    where go [] = error "invalid flag"
          go [c] = App.short c
          go w = App.long w

switch :: forall (flags :: [Symbol]) (description :: Symbol). Optional (Flag flags description) () -> Bool
switch = fromMaybe False . flag @flags @description True . optional

option :: forall (flags :: [Symbol]) (placeholder :: Symbol) (description :: Symbol) a. Optional (Flagged flags placeholder description) a -> Maybe a
option = flagged @flags @placeholder @description . optional
