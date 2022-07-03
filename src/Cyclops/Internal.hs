{-# LANGUAGE
    AllowAmbiguousTypes
  , BlockArguments
  , LambdaCase
  , MultiParamTypeClasses
  , StandaloneKindSignatures
#-}
module Cyclops.Internal where

import Data.Proxy (Proxy)
import GHC.Types (Type, Constraint)
import System.Exit (ExitCode)

getOpsFrom :: Ops m t => Version -> Description -> ProgName -> [String] -> m (Either OpsException t)
getOpsFrom = undefined

type Ops :: (Type -> Type) -> Type -> Constraint
class Ops m t where
  tuple :: Proxy t
  effects :: Proxy m

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
