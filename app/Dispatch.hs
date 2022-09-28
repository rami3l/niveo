module Dispatch
  ( args,
    dispatch,
  )
where

import Data.Default (Default (..))
import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static
import Error.Diagnose (Diagnostic)
import Error.Diagnose.Diagnostic (printDiagnostic)
import Error.Diagnose.Style (defaultStyle)
import Niveo.Interpreter (Context (..), interpret)
import Options.Applicative
  ( Parser,
    help,
    long,
    metavar,
    short,
    strOption,
    switch,
  )
import System.Console.Haskeline
  ( InputT,
    defaultSettings,
    getInputLine,
    outputStrLn,
    runInputT,
  )
import Prelude hiding (Reader, runReader)

data Args = Args
  { fin :: Maybe FilePath,
    repl :: Bool
  }

args :: Parser Args
args = Args <$> optional (strOption load) <*> switch repl'
  where
    load =
      long "load"
        <> short 'f'
        <> metavar "FILE"
        <> help "Load a source file"
    repl' = long "repl" <> short 'i' <> help "REPL (interactive) mode"

dispatch :: Args -> IO ()
dispatch (Args fin repl') = do
  -- fin `whenJust` \fin' -> undefined -- TODO: load fin'
  when (isNothing fin || repl') repl

repl :: IO ()
repl = runInputT defaultSettings loop
  where
    loop = do
      getInputLine ">> " >>= (`whenJust` interpret')
      loop
    interpret' ln = res & either (liftIO . printErr) print'
      where
        res = runPureEff . runReader ctx . runErrorNoCallStack $ interpret
        ctx = Context {env = def, fin = "<stdin>", src = toText ln}
        print' = outputStrLn . ("<< " <>) . show @String
        printErr = printDiagnostic stdout useUnicode useColors 4 defaultStyle
        (useUnicode, useColors) = (True, True)
