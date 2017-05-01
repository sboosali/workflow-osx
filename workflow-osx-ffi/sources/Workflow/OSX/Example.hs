{-# OPTIONS_GHC -fno-warn-missing-signatures #-} -- to test inference
module Workflow.OSX.Example where
import Workflow.OSX()
import System.Environment

{-|
@
stack build && stack exec -- example-workflow-osx-ffi
@
-}
main :: IO ()
main = do
 arguments <- getArgs >>= \case
  [s] -> return (s)
  _ -> return ("")
 mainWith arguments

mainWith s = do
 putStrLn s
 putStrLn "(Workflow.OSX.Example...)"

