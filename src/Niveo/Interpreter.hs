module Niveo.Interpreter where

import Data.Default (Default (def))
import Data.Either.Extra (mapLeft, maybeToEither)
import Data.String.Interpolate
import Data.Vector (Vector)
import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local
import Error.Diagnose (Marker (This), addFile, addReport, err)
import Error.Diagnose.Diagnostic (Diagnostic)
import Error.Diagnose.Report (Report)
import Niveo.Parser (Expr (..), Prog (..), Token (..), TokenType (..), parse, program)
import Optics.TH (makeFieldLabelsNoPrefix)
import Relude hiding (State, get)

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

interpret ::
  (Error (Diagnostic Text) :> es, State Context :> es) =>
  Eff es Val
interpret = do
  ctx <- get @Context
  prog <- parse program ctx.fin ctx.src & either throwError pure
  eval prog.expr

eval ::
  (Error (Diagnostic Text) :> es, State Context :> es) =>
  Expr ->
  Eff es Val
eval expr = do
  ctx <- get @Context
  let diag = addFile @Text def ctx.fin $ toString ctx.src
  throwError diag

-- eval (EUnary op rhs) = do
--   rhs' <- eval rhs
--   case (op.type_, rhs') of
--     (TBang, VBool b) -> pure . VBool $ not b
--     (TPlus, VNum n) -> pure . VNum $ n
--     (TMinus, VNum n) -> pure . VNum $ negate n
--     _ ->
--       throwE $
--         err
--           Nothing
--           "Type mismatch"
--           [(op.range, This [i|Could not apply `#{op}` to `#{rhs'}`|])]
--           []

-- eval (EVar Token {lexeme}) = do
--   def <- get <&> (Map.!? lexeme)
--   def & maybeToEither [i|undefined variable `#{lexeme}`|] & except
