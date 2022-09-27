module Niveo.Interpreter
  ( Val (..),
    Env,
    Context (..),
    Store (..),
    interpret,
    eval,
  )
where

import Data.Default (Default (def))
import Data.String.Interpolate
import Data.Vector (Vector)
import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static
import Effectful.State.Static.Local
import Error.Diagnose (Marker (This), addFile, addReport, err)
import Error.Diagnose.Diagnostic (Diagnostic)
import Niveo.Parser (Expr (..), Prog (..), Token (..), TokenType (..), parse, program)
import Optics.TH (makeFieldLabelsNoPrefix)
import Prelude hiding (Reader, State, ask, get)

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
  deriving (Eq)

-- TODO: Add proper pretty-printing for `Val`.
deriving instance Show Val

newtype Store = Store
  { env :: Env
  }

type Env = HashMap Text Val

makeFieldLabelsNoPrefix ''Store

data Context = Context
  { fin :: FilePath,
    src :: Text
  }

makeFieldLabelsNoPrefix ''Context

interpret ::
  (Error (Diagnostic Text) :> es, Reader Context :> es, State Store :> es) =>
  Eff es Val
interpret = do
  ctx <- ask @Context
  prog <- parse program ctx.fin ctx.src & either throwError pure
  eval prog.expr

eval ::
  (Error (Diagnostic Text) :> es, Reader Context :> es, State Store :> es) =>
  Expr ->
  Eff es Val
eval expr = do
  ctx <- ask @Context
  let diag = addFile @Text def ctx.fin $ toString ctx.src
  let throwReport msg markers = throwError . (diag `addReport`) $ err Nothing msg markers []
  case expr of
    (EUnary op rhs) -> do
      rhs' <- eval rhs
      case (op.type_, rhs') of
        (TBang, VBool b) -> pure . VBool $ not b
        (TPlus, VNum x) -> pure . VNum $ x
        (TMinus, VNum x) -> pure . VNum $ negate x
        _ ->
          throwReport
            "Type mismatch"
            [(op.range, This [i|Could not apply `#{op}` to `#{rhs'}`|])]
    (EBinary lhs op rhs) -> do
      (lhs', rhs') <- (,) <$> eval lhs <*> eval rhs
      case (op.type_, lhs', rhs') of
        (TStar2, VNum x, VNum y) -> pure . VNum $ x ** y
        (TSlash, VNum x, VNum y) -> pure . VNum $ x / y
        (TStar, VNum x, VNum y) -> pure . VNum $ x * y
        (TMinus, VNum x, VNum y) -> pure . VNum $ x - y
        (TPlus, VNum x, VNum y) -> pure . VNum $ x + y
        (TPlus, VStr x, VStr y) -> pure . VStr $ x <> y
        (TPlus, VList x, VList y) -> pure . VList $ x <> y
        (TGtEq, VNum x, VNum y) -> pure . VBool $ x >= y
        (TGt, VNum x, VNum y) -> pure . VBool $ x > y
        (TLtEq, VNum x, VNum y) -> pure . VBool $ x <= y
        (TLt, VNum x, VNum y) -> pure . VBool $ x < y
        (TBangEq, x, y) -> pure . VBool $ x /= y
        (TEq2, x, y) -> pure . VBool $ x == y
        (TAmp2, VBool x, VBool y) -> pure . VBool $ x && y
        (TPipe2, VBool x, VBool y) -> pure . VBool $ x || y
        _ ->
          throwReport
            "Type mismatch"
            [(op.range, This [i|Could not apply `#{op}` to `(#{lhs'}, #{rhs'})`|])]
    _ ->
      throwReport
        "Unsupported operation"
        []

-- eval (EVar Token {lexeme}) = do
--   def <- get <&> (Map.!? lexeme)
--   def & maybeToEither [i|undefined variable `#{lexeme}`|] & except
