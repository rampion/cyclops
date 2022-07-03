{-# LANGUAGE
    BlockArguments
  , FlexibleContexts
  , TypeApplications
#-}
module Main where

import Data.Functor.Identity (Identity(..))
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
        getOpsTest [] `shouldBe` Right ()

      it "complains if given more arguments" do
        getOpsTest @() ["one"] `shouldBe` Left do
          ExitWith
            do ExitFailure 1
            do "Invalid argument `one'\n\
               \\n\
               \Usage: example [-v|--version]"
