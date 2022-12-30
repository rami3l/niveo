module Niveo.Interpreter.Utils (diagnosticFromCtx, throwReport) where

import Data.Default
import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static
import Error.Diagnose (Diagnostic, Marker, Position, Report (Err), addFile, addReport)
import Niveo.Interpreter.Types (Context (..))
import Prelude hiding (Reader, ask, asks)

diagnosticFromCtx :: Context -> Text -> [(Position, Marker Text)] -> Diagnostic Text
diagnosticFromCtx ctx msg markers =
  let diag = addFile @Text def ctx.fin $ toString ctx.src
   in diag `addReport` Err Nothing msg markers []

throwReport ::
  [Error (Diagnostic Text), Reader Context] :>> es =>
  Text ->
  [(Position, Marker Text)] ->
  Eff es a
throwReport msg markers = ask >>= \ctx -> throwError $ diagnosticFromCtx ctx msg markers
