module Dispatch
  ( args,
    dispatch,
  )
where

import Data.Default (Default (..))
import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static
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
  )
import System.Console.Haskeline
  ( InputT,
    defaultSettings,
    getInputLine,
    outputStrLn,
    runInputT,
  )
import Prelude hiding (Reader, runReader)

newtype Args = Args
  { fin :: Maybe FilePath
  }

args :: Parser Args
args = Args <$> optional (strOption load)
  where
    load =
      long "load"
        <> short 'f'
        <> metavar "FILE"
        <> help "Load a source file"

dispatch :: Args -> IO ()
dispatch (Args fin) = fin & maybe repl (runInputT defaultSettings . interpret')

repl :: IO ()
repl = runInputT defaultSettings $ fix \loop -> do
  getInputLine ">> " >>= (`whenJust` interpret')
  loop

interpret' :: MonadIO m => String -> InputT m ()
interpret' ln = res & either (liftIO . printErr) print'
  where
    res = runPureEff . runReader ctx . runErrorNoCallStack $ interpret
    ctx = Context {env = def, fin = "<stdin>", src = toText ln}
    print' = outputStrLn . ("<< " <>) . show @String
    printErr = printDiagnostic stdout useUnicode useColors 4 defaultStyle
    (useUnicode, useColors) = (True, True)
