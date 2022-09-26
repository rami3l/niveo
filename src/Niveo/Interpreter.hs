module Niveo.Interpreter where

import Control.Monad.Trans.Except (except, mapExceptT, throwE)
import Data.Default (Default (def))
import Data.Either.Extra (mapLeft, maybeToEither)
import Data.String.Interpolate
import Data.Vector (Vector)
import Error.Diagnose (Marker (This), addFile, addReport, err)
import Error.Diagnose.Diagnostic (Diagnostic)
import Error.Diagnose.Report (Report)
import Niveo.Parser (Expr (..), Prog (..), Token (..), TokenType (..), parse, program)
import Optics.TH (makeFieldLabelsNoPrefix)
import Relude

data Val
  = -- Native JSON types.
    VNull
  | VBool !Bool
  | VNum !Double
  | VStr !Text
  | VList (Vector Val)
  | -- | `VStruct` is encoded as a `Vector` of @(tag, value)@ pairs,
    -- where the `tag` should be a `VStr` or a `VAtom`.
    VStruct (Vector (Val, Val))
  | -- Extra types.
    VAtom !Text
  | VLambda {params :: !Token, body :: Expr}

-- TODO: Add proper pretty-printing for `Val`.
deriving instance Show Val

type Env = HashMap Text Expr

data Context = Context {env :: Env, fin :: FilePath, src :: Text}

makeFieldLabelsNoPrefix ''Context

interpret :: ExceptT (Diagnostic Text) (State Context) Val
interpret = do
  ctx <- get
  prog <- except $ parse program ctx.fin ctx.src
  let diag = def `addFile` ctx.fin $ toString ctx.src
  eval prog.expr & mapExceptT (<&> mapLeft (diag `addReport`))

eval :: Expr -> ExceptT (Report Text) (State Context) Val
eval (EUnary op rhs) = do
  rhs' <- eval rhs
  case (op.type_, rhs') of
    (TBang, VBool b) -> pure . VBool $ not b
    (TPlus, VNum n) -> pure . VNum $ n
    (TMinus, VNum n) -> pure . VNum $ negate n
    _ ->
      throwE $
        err
          Nothing
          "Type mismatch"
          [(op.range, This [i|Could not apply `#{op}` to `#{rhs'}`|])]
          []

-- eval (EVar Token {lexeme}) = do
--   def <- get <&> (Map.!? lexeme)
--   def & maybeToEither [i|undefined variable `#{lexeme}`|] & except
