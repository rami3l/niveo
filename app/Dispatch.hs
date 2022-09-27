module Dispatch
  ( args,
    dispatch,
  )
where

import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static
import Effectful.State.Static.Local
import Error.Diagnose (Diagnostic)
import Niveo.Interpreter (Context, Store)
import Options.Applicative
  ( Parser,
    help,
    long,
    metavar,
    short,
    strOption,
    switch,
  )
import Relude hiding (Reader, State, ask, get)
import System.Console.Haskeline (InputT)

data Args = Args
  { fin :: Maybe FilePath,
    repl :: Bool
  }

args :: Parser Args
args =
  let load =
        strOption $
          long "load"
            <> short 'f'
            <> metavar "FILE"
            <> help "Load a source file"
      repl = long "repl" <> short 'i' <> help "REPL (interactive) mode"
   in Args
        <$> optional load
        <*> switch repl

dispatch :: Args -> IO ()
dispatch (Args fin repl) = do
  -- TODO: Read `fin` (if exists) and then launch REPL (if `repl`).
  undefined

repl :: (IOE :> es, Reader Context :> es, State Store :> es) => InputT (Eff es) ()
repl = undefined
