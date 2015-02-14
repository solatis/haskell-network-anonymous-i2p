module Main where

import Test.Hspec.Runner
import qualified Spec

import Network (withSocketsDo)

main :: IO ()
main =
  withSocketsDo $ hspecWith defaultConfig Spec.spec
