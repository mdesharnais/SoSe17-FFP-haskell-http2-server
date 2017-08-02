import qualified Test.HUnit as HUnit
import qualified TestHuffman
import qualified TestHpack

import System.Exit(exitFailure, exitSuccess)
import Test.QuickCheck
import Test.QuickCheck.Instances

main :: IO ()
main = do
  counts <- HUnit.runTestTT (HUnit.test [ TestHpack.hunitTests ])
  if HUnit.errors counts /= 0 || HUnit.failures counts /= 0 then
    exitFailure
  else
    return ()
  putStrLn ""
  success <- (&&) <$> TestHpack.quickcheckTests <*> TestHuffman.tests
  if success then
    exitSuccess
  else
    exitFailure
