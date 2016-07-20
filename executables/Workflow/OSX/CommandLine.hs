module Main where
-- module Workflow.OSX.CommandLine where

import Workflow.Parser (cmdln)
import Workflow.OSX

import Data.Default.Class

{- |

@
stack build && stack exec -- test-workflow-osx
@

-}
main = do
 cmdln (runWorkflowT def)

