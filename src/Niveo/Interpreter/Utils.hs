module Niveo.Interpreter.Utils (throwReport) where

import Data.Default
import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static
import Error.Diagnose (Diagnostic, Marker, Position, addReport, err)
import Error.Diagnose.Diagnostic (addFile)
import Niveo.Interpreter.Types (Context (..))
import Prelude hiding (Reader, ask)

throwReport ::
  [Error (Diagnostic Text), Reader Context] :>> es =>
  Text ->
  [(Position, Marker Text)] ->
  Eff es a
throwReport msg markers = do
  ctx <- ask @Context
  let diag = addFile @Text def ctx.fin $ toString ctx.src
  throwError . (diag `addReport`) $ err Nothing msg markers []
