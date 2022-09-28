module Niveo.Interpreter
  ( Val (..),
    Env,
    Context (..),
    interpret,
    eval,
  )
where

import Data.Default (Default (def))
import Data.HashMap.Strict qualified as HashMap
import Data.String.Interpolate
import Data.Vector (Vector)
import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static
import Error.Diagnose (Marker (This), addFile, addReport, err)
import Error.Diagnose.Diagnostic (Diagnostic)
import Niveo.Parser
  ( Expr (..),
    Lit (..),
    Prog (..),
    Token (..),
    TokenType (..),
    parse,
    program,
  )
import Optics.TH (makeFieldLabelsNoPrefix)
import Relude.Unsafe (read)
import Witch
import Prelude hiding (Reader, ask)

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
  | VLambda {params :: !Token, body :: Expr, env :: Env}
  deriving (Eq)

instance From Lit Val where
  from LNull = VNull
  from (LBool b) = VBool b
  from (LNum x) = VNum . read . into $ x
  from (LStr s) = VStr s
  from (LAtom s) = VAtom s

-- TODO: Add proper pretty-printing for `Val`.
deriving instance Show Val

newtype Env = Env {dict :: HashMap Text Val} deriving (Eq, Show)

instance Default Env where
  def = Env {dict = HashMap.empty}

data Context = Context
  { env :: Env,
    fin :: FilePath,
    src :: Text
  }
  deriving (Generic)

makeFieldLabelsNoPrefix ''Context

interpret ::
  [Error (Diagnostic Text), Reader Context] ~ es =>
  Eff es Val
interpret = do
  ctx <- ask @Context
  prog <- parse program ctx.fin ctx.src & either throwError pure
  eval prog.expr

eval ::
  [Error (Diagnostic Text), Reader Context] :>> es =>
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
    (ELit l) -> pure (from l)
    _ ->
      -- TODO: Remove this.
      throwReport
        [i|Unimplemented operation `#{expr}`|]
        []

-- eval (EVar Token {lexeme}) = do
--   def <- get <&> (Map.!? lexeme)
--   def & maybeToEither [i|undefined variable `#{lexeme}`|] & except
