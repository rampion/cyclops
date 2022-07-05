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

import Data.Function ((&))
import Data.Functor.Identity (Identity(..))
import Data.List.NonEmpty (NonEmpty(..))
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

        shouldMatch ::
          forall t. (Show t, Typeable t) => 
          (Expectation -> OpsException -> Expectation) ->
          (Expectation -> t -> Expectation) -> 
          Either OpsException t -> Expectation
        shouldMatch lft rgt val = 
          val & either 
            do lft do expectationFailure do "expected Right (_ :: " ++ show (typeRep do Proxy @t) ++ ") but got " ++ show val
            do rgt do expectationFailure do "expected Left (_ :: OpsException) but got " ++ show val
          
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
        getOpsTest [] & shouldMatch const \_ () -> pure ()

      it "complains if given more arguments" do
        getOpsTest @() ["one"] `shouldBe` Left do
          ExitWith
            do ExitFailure 1
            do "Invalid argument `one'\n\
               \\n\
               \Usage: example [-v|--version]"

    describe "instance Ops m Arg" do
      it "parses one argument successfully" do
        getOpsTest ["one"] & shouldMatch const \_
          (arg @"placeholder" @"description" -> txt) -> txt `shouldBe` "one"

      it "fails to parse a flag" do
        getOpsTest ["--one"] & shouldMatch 
          do \_ -> \case
              ExitWith (ExitFailure 1) (takeWhile (/='\n') -> msg) -> msg `shouldBe` "Invalid option `--one'"
              val -> expectationFailure do "expected ExitWith (ExitFailure 1) \"Invalid option `--one'\\n…\" but got " ++ show val
          do \fail (arg @"placeholder" @"description" -> _ :: String) -> fail

      it "lists the placeholder and description correctly in --help" do
        getOpsTest ["--help"] & shouldMatch
          do \_ exc -> exc `shouldBe` ExitWith
              do ExitSuccess
              do "Usage: example [-v|--version] xxx\n\
                 \\n\
                 \Available options:\n\
                 \  -h,--help                Show this help text\n\
                 \  -v,--version             Show the version (1.0.0) and exit\n\
                 \  xxx                      Three 'x's\n\
                 \\n\
                 \An example program."
          do \fail (arg @"xxx" @"Three 'x's" -> _ :: String) -> fail

    describe "instance Ops m Readable" do
      it "parses one Int successfully" do
        getOpsTest ["12345"] & shouldMatch const \_
          (arg @"placeholder" @"description" . readable -> n) -> n `shouldBe` (12345 :: Int)

    describe "instance Ops m Optional" do
      it "parses one arg successfully" do
        getOpsTest ["one"] & shouldMatch const \_
          (arg @"placeholder" @"description" . optional -> n) -> n `shouldBe` Just "one"

      it "parses one flag successfully" do
        getOpsTest ["--one"] & shouldMatch const \_
          (flag @'["one","o"] @"description" () . optional -> n) -> n `shouldBe` Just ()

      it "parses no args successfully" do
        getOpsTest [] & shouldMatch const \_
          (arg @"placeholder" @"description" . optional -> n) -> n `shouldBe` Nothing @String

      it "marks the placeholder as optional in --help" do
        getOpsTest ["--help"] & shouldMatch
          do \_ exc -> exc `shouldBe` ExitWith
              do ExitSuccess
              do "Usage: example [-v|--version] [xxx]\n\
                 \\n\
                 \Available options:\n\
                 \  -h,--help                Show this help text\n\
                 \  -v,--version             Show the version (1.0.0) and exit\n\
                 \  xxx                      Three 'x's\n\
                 \\n\
                 \An example program."
          do \fail (arg @"xxx" @"Three 'x's" . optional -> _ :: Maybe String) -> fail

    describe "instance Ops m DefaultTo" do
      it "parses one arg successfully" do
        getOpsTest ["one"] & shouldMatch const \_
          (arg @"placeholder" @"description" . defaultTo @"default" -> n) -> n `shouldBe` "one"

      it "parses no args successfully" do
        getOpsTest [] & shouldMatch const \_
          (arg @"placeholder" @"description" . defaultTo @"default" -> n) -> n `shouldBe` "default"

      it "errors on a bad default" do
        let unparsableDefault = getOpsTest [] & shouldMatch const \_
              (arg @"placeholder" @"description" . readable . defaultTo @"default" -> n) -> n `shouldBe` (123 :: Int)
        unparsableDefault `shouldThrow` errorCall "Unparseable default value \"default\""

      it "records the default in --help" do
        pendingWith "need to be able to specify inner parser Mods"

        getOpsTest ["--help"] & shouldMatch
          do \_ exc -> exc `shouldBe` ExitWith
              do ExitSuccess
              do "Usage: example [-v|--version] [xxx]\n\
                 \\n\
                 \Available options:\n\
                 \  -h,--help                Show this help text\n\
                 \  -v,--version             Show the version (1.0.0) and exit\n\
                 \  xxx                      Three 'x's (default: something) \n\
                 \\n\
                 \An example program."
          do \fail (arg @"xxx" @"Three 'x's" . defaultTo @"something" -> _ :: String) -> fail

    describe "instance Ops m ZeroOrMore" do
      it "parses one arg successfully" do
        getOpsTest ["one"] & shouldMatch const \_
          (arg @"placeholder" @"description" . zeroOrMore -> n) -> n `shouldBe` ["one"]

      it "parses no args successfully" do
        getOpsTest [] & shouldMatch const \_
          (arg @"placeholder" @"description" . zeroOrMore -> n) -> n `shouldBe` [] @String

      it "parses four args successfully" do
        getOpsTest ["one","two","three","four"] & shouldMatch const \_
          (arg @"placeholder" @"description" . zeroOrMore -> n) -> n `shouldBe` ["one","two","three","four"]

      it "marks the placeholder appropriately in --help" do
        pendingWith "need to be able to specify inner parser Mods"

        getOpsTest ["--help"] & shouldMatch
          do \_ exc -> exc `shouldBe` ExitWith
              do ExitSuccess
              do "Usage: example [-v|--version] [xxx ...]\n\
                 \\n\
                 \Available options:\n\
                 \  -h,--help                Show this help text\n\
                 \  -v,--version             Show the version (1.0.0) and exit\n\
                 \  xxx                      Three 'x's\n\
                 \\n\
                 \An example program."
          do \fail (arg @"xxx" @"Three 'x's" . zeroOrMore -> _ :: [String]) -> fail

    describe "instance Ops m OneOrMore" do
      it "parses one arg successfully" do
        getOpsTest ["one"] & shouldMatch const \_
          (arg @"placeholder" @"description" . oneOrMore -> n) -> n `shouldBe` ("one" :| [])

      it "fails to parse no args" do
        getOpsTest [] & shouldMatch
          do \_ -> \case
              ExitWith (ExitFailure 1) msg -> takeWhile (/='\n') msg `shouldBe` "Missing: xxx"
              val -> expectationFailure do "expected ExitWith (ExitFailure 1) \"Missing: xxx\\n…\" but got " ++ show val
          do \fail (arg @"xxx" @"description" . oneOrMore -> _ :: NonEmpty String) -> fail

      it "parses four args successfully" do
        getOpsTest ["one","two","three","four"] & shouldMatch const \_
          (arg @"placeholder" @"description" . oneOrMore -> n) -> n `shouldBe` ("one" :| ["two","three","four"])

      it "marks the placeholder appropriately in --help" do
        pendingWith "need to be able to specify inner parser Mods"

        getOpsTest ["--help"] & shouldMatch
          do \_ exc -> exc `shouldBe` ExitWith
              do ExitSuccess
              do "Usage: example [-v|--version] [xxx ...]\n\
                 \\n\
                 \Available options:\n\
                 \  -h,--help                Show this help text\n\
                 \  -v,--version             Show the version (1.0.0) and exit\n\
                 \  xxx                      Three 'x's\n\
                 \\n\
                 \An example program."
          do \fail (arg @"xxx" @"Three 'x's" . oneOrMore -> _ :: NonEmpty String) -> fail

    describe "instance Ops m Flag" do
      it "parses one long flag successfully" do
        getOpsTest ["--one"] & shouldMatch const \_
          (flag @["one","o"] @"description" "YES" -> x) -> x `shouldBe` "YES"

      it "parses one short flag successfully" do
        getOpsTest ["-o"] & shouldMatch const \_
          (flag @["one","o"] @"description" "YES" -> x) -> x `shouldBe` "YES"

      it "fails to parse no args" do
        getOpsTest [] & shouldMatch
          do \_ -> \case
              ExitWith (ExitFailure 1) msg -> takeWhile (/='\n') msg `shouldBe` "Missing: (-o|--one)"
              val -> expectationFailure do "expected ExitWith (ExitFailure 1) \"Missing: (-o|--one)\\n…\" but got " ++ show val
          do \fail (flag @["one","o"] @"description" id -> _) -> fail

      it "documents the flags in --help" do
        getOpsTest ["--help"] & shouldMatch
          do \_ exc -> exc `shouldBe` ExitWith
              do ExitSuccess
              do "Usage: example [-v|--version] (-o|--one)\n\
                 \\n\
                 \Available options:\n\
                 \  -h,--help                Show this help text\n\
                 \  -v,--version             Show the version (1.0.0) and exit\n\
                 \  -o,--one                 description\n\
                 \\n\
                 \An example program."
          do \fail (flag @["one","o"] @"description" "YES" -> _) -> fail

    describe "instance Ops m Flagged" do
      it "parses one long flagged argument successfully" do
        getOpsTest ["--one", "steve"] & shouldMatch const \_
          (flagged @["one","o"] @"placeholder" @"description" -> x) -> x `shouldBe` "steve"

      it "fails to parse no args" do
        getOpsTest [] & shouldMatch
          do \_ -> \case
              ExitWith (ExitFailure 1) msg -> takeWhile (/='\n') msg `shouldBe` "Missing: (-o|--one placeholder)"
              val -> expectationFailure do "expected ExitWith (ExitFailure 1) \"Missing: (-o|--one)\\n…\" but got " ++ show val
          do \fail (flagged @["one","o"] @"placeholder" @"description" -> _ :: String) -> fail

    describe "switch" do
      it "parses the presence of a flag as True" do
        getOpsTest ["--debug"] & shouldMatch const \_
          (switch @'["d", "debug"] @"Show debugging statements" -> b) -> b `shouldBe` True

      it "parses the absence of a flag as False" do
        getOpsTest [] & shouldMatch const \_
          (switch @'["d", "debug"] @"Show debugging statements" -> b) -> b `shouldBe` False

    describe "option" do
      it "parses the presence of a flagged argument as Just the parsed value" do
        getOpsTest ["--dictionary", "/usr/share/dict/words"] & shouldMatch const \_
          (option @'["--dictionary"] @"PATH" @"File to draw words from" -> b) -> b `shouldBe` Just "/usr/share/dict/words"

      it "parses the absence of a flagged argument as Nothing" do
        getOpsTest [] & shouldMatch const \_
          (option @'["--dictionary"] @"PATH" @"File to draw words from" -> b) -> b `shouldBe` Nothing @String
