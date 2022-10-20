module Dispatch
  ( args,
    dispatch,
  )
where

import Control.Applicative.Combinators (choice)
import Control.Monad.Catch (MonadCatch, catch)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as AesonPretty
import Data.Default (Default (..))
import Data.String.Interpolate
import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static
import Error.Diagnose (Diagnostic, printDiagnostic)
import Error.Diagnose.Style (defaultStyle)
import Niveo.Interpreter (Context (..), Val, evalTxt, throwReport)
import Niveo.Interpreter.FileSystem (FsError (FsError), runFileSystemIO)
import Niveo.Interpreter.Types ()
import Niveo.Interpreter.Utils (diagnosticFromCtx)
import Options.Applicative
  ( Parser,
    flag',
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
import UnliftIO (StringException (StringException), fromEither, stringException)
import Witch
import Prelude hiding (Reader, runReader)

dispatch :: Args -> IO ()
dispatch a = case a.mode of
  ALoad fin -> runTxt . decodeUtf8 =<< readFileBS fin
  ACmd txt -> runTxt txt
  AREPL -> repl
  where
    runInputT' = runInputT defaultSettings
    runTxt = runInputT' . evalStrInput

    repl = runInputT' $ fix \loop -> do
      getInputLine ">> " >>= (`whenJust` evalStrInput)
      loop

    evalStrInput :: (MonadIO m, MonadCatch m) => String -> InputT m ()
    evalStrInput ln = runEff evalStrIO & liftIO >>= either printErr print'
      where
        evalStrIO :: Eff '[IOE] (Either (Diagnostic Text) Val) =
          evalTxt
            & runFileSystemIO
            & runErrorNoCallStack @FsError
            >>= either (\(FsError e) -> throwReport e []) pure
            & runErrorNoCallStack @(Diagnostic _)
            & runReader ctx
        ctx = Context {env = def, fin = "<stdin>", src = toText ln}

        print' v = do
          when isREPL . outputStrLn $ "<< " <> show @String v
          case a.exportFmt of
            Nothing -> unless isREPL . outputStrLn $ show @String v
            Just JSON ->
              (intoAeson v >>= putLBSLn . AesonPretty.encodePretty)
                `catch` \(StringException s _) -> do
                  printErr $ diagnosticFromCtx ctx (into s) []
                  unless isREPL exitFailure
        intoAeson =
          fromEither
            . first (\(TryFromException v _) -> stringException [i|value cannot be exported: #{v}|])
            . tryInto @Aeson.Value
        isREPL = a.mode == AREPL

        printErr = printDiagnostic stderr useUnicode useColors 4 defaultStyle
        (useUnicode, useColors) = (True, True)

data Args = Args
  { mode :: AMode,
    exportFmt :: Maybe ExportFormat
  }

args :: Parser Args
args = Args <$> mode <*> optional exportFmt

data AMode
  = ALoad FilePath
  | ACmd String
  | AREPL
  deriving (Eq)

mode :: Parser AMode
mode =
  choice
    [ ALoad
        <$> strOption
          ( long "run"
              <> short 'f'
              <> metavar "FILE"
              <> help "Load and interpret FILE"
          ),
      ACmd
        <$> strOption
          ( long "cmd"
              <> short 'c'
              <> metavar "SCRIPT"
              <> help "Interpret the given SCRIPT string"
          ),
      pure AREPL
    ]

data ExportFormat = JSON

exportFmt :: Parser ExportFormat
exportFmt = flag' JSON (long "export-json" <> long "json" <> help "Enable JSON output")
