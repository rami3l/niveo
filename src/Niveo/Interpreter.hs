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
import Niveo.Instances ()
import Niveo.Parser
  ( Expr (..),
    Lit (..),
    LitType (..),
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
  from (Lit LNull _) = VNull
  from (Lit LBool b) = VBool $ b.lexeme == "true"
  from (Lit LNum x) = VNum . read . into $ x.lexeme
  from (Lit LStr s) = VStr s.lexeme
  from (Lit LAtom s) = VAtom s.lexeme

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
            [(expr.range, This [i|Could not apply `#{op}` to `#{rhs'}`|])]
    (EBinary lhs op rhs) -> do
      -- Lazy evaluation! No need to worry about short-circuiting.
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
            "mismatched types"
            [(expr.range, This [i|could not apply `#{op}` to `(#{lhs'}, #{rhs'})`|])]
    -- ECall
    -- EIndex
    (EParen x _) -> eval x
    (EList xs _) -> VList . from <$> eval `traverse` xs
    (EIfElse _ cond then' else') ->
      eval cond >>= \case
        (VBool cond') -> eval $ if cond' then then' else else'
        cond' -> throwReport "mismatched types" [(cond.range, This [i|expected boolean condition, found `#{cond'}`|])]
    (ELit l) -> pure (from l)
    _ ->
      -- TODO: Remove this.
      throwReport
        [i|unimplemented operation `#{expr}`|]
        []

-- eval (EVar Token {lexeme}) = do
--   def <- get <&> (Map.!? lexeme)
--   def & maybeToEither [i|undefined variable `#{lexeme}`|] & except
