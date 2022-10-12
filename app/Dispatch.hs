module Dispatch
  ( args,
    dispatch,
  )
where

import Data.Default (Default (..))
import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static
import Error.Diagnose.Diagnostic (Diagnostic, printDiagnostic)
import Error.Diagnose.Style (defaultStyle)
import Niveo.Interpreter (Context (..), Val, evalTxt, throwReport)
import Niveo.Interpreter.FileSystem (FsError (FsError), runFileSystemIO)
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
dispatch (Args {fin}) = fin & maybe repl (runInputT defaultSettings . evalTxtInput)

repl :: IO ()
repl = runInputT defaultSettings $ fix \loop -> do
  getInputLine ">> " >>= (`whenJust` evalTxtInput)
  loop

evalTxtInput :: MonadIO m => String -> InputT m ()
evalTxtInput ln =
  runEff evalTxtIO & liftIO >>= either printErr print'
  where
    evalTxtIO :: Eff '[IOE] (Either (Diagnostic Text) Val) =
      evalTxt
        & runFileSystemIO
        & runErrorNoCallStack @FsError
        >>= either (\(FsError e) -> throwReport e []) pure
        & runErrorNoCallStack @(Diagnostic _)
        & runReader ctx
    ctx = Context {env = def, fin = "<stdin>", src = toText ln}
    print' = outputStrLn . ("<< " <>) . show @String
    printErr = printDiagnostic stdout useUnicode useColors 4 defaultStyle
    (useUnicode, useColors) = (True, True)
