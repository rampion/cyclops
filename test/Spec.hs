{-# LANGUAGE
    BlockArguments
  , DataKinds
  , FlexibleContexts
  , LambdaCase
  , Rank2Types
  , ScopedTypeVariables
  , TypeApplications
  , ViewPatterns
#-}
module Main where

import Data.Functor.Identity (Identity(..))
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable, typeRep)
import System.Exit (ExitCode(..))

import Test.Hspec
import Cyclops.Internal

main :: IO ()
main = hspec do
  describe "Cyclops.Internal" do
    let getOpsTest :: Ops Identity t => [String] -> Either OpsException t
        getOpsTest = runIdentity . getOpsFrom vers desc name

        name = ProgName "example"
        vers = Version "1.0.0"
        desc = Description "An example program."

        shouldMatch :: forall t. Typeable t => Either OpsException t -> (t -> Expectation) -> Expectation
        shouldMatch = \case
          Right val -> \f -> f val 
          Left err -> \_ -> expectationFailure 
            do "expected " ++ show (typeRep do Proxy @t) ++ " but got " ++ show err
          
        (<&) :: a -> (a -> ()) -> a
        (<&) = const

    describe "getOpsFrom" do
      it "shows the usage when given --help" do
        getOpsTest @() ["--help"] `shouldBe` Left do
          ExitWith
            do ExitSuccess
            do "Usage: example [-v|--version]\n\
               \\n\
               \Available options:\n\
               \  -h,--help                Show this help text\n\
               \  -v,--version             Show the version (1.0.0) and exit\n\
               \\n\
               \An example program."

      it "shows the version when given --version" do
        getOpsTest @() ["--version"] `shouldBe` Left do
          ExitWith
            do ExitSuccess
            do "example 1.0.0"

    describe "instance Ops m ()" do
      it "parses no arguments successfully" do
        getOpsTest [] `shouldMatch` \case
          () -> pure ()

      it "complains if given more arguments" do
        getOpsTest @() ["one"] `shouldBe` Left do
          ExitWith
            do ExitFailure 1
            do "Invalid argument `one'\n\
               \\n\
               \Usage: example [-v|--version]"

    describe "instance Ops m Arg" do
      it "parses one argument successfully" do
        getOpsTest ["one"] `shouldMatch` \case
          (arg @"placeholder" @"description" -> txt) -> txt `shouldBe` "one"

      it "lists the placeholder and description correctly in --help" do
        let actual = getOpsTest ["--help"] <& \case
              Right (arg @"xxx" @"Three 'x's" -> _ :: String)  -> ()
              Left _                                          -> ()

        actual `shouldBe` Left do
          ExitWith
            do ExitSuccess
            do "Usage: example [-v|--version] xxx\n\
               \\n\
               \Available options:\n\
               \  -h,--help                Show this help text\n\
               \  -v,--version             Show the version (1.0.0) and exit\n\
               \  xxx                      Three 'x's\n\
               \\n\
               \An example program."

    describe "instance Ops m Readable" do
      it "parses one Int successfully" do
        getOpsTest ["12345"] `shouldMatch` \case
          (arg @"placeholder" @"description" . readable -> n) -> n `shouldBe` (12345 :: Int)

