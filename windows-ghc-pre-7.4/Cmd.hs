module Cmd where
import System.Cmd
system s = System.Cmd.system ("\"" ++ s ++ "\"")
