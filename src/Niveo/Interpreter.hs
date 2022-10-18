{-# OPTIONS_GHC -Wno-orphans #-}

module Niveo.Interpreter
  ( Val (..),
    Env,
    Context (..),
    evalTxt,
    eval,
    throwReport,
  )
where

import Control.Monad (foldM)
import Control.Monad.Fix (mfix)
import Data.Default (Default (def))
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.String.Interpolate
import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static
import Error.Diagnose (Marker (This))
import Niveo.Instances ()
import Niveo.Interpreter.FileSystem (readFile)
import Niveo.Interpreter.Types
import Niveo.Interpreter.Utils (throwReport)
import Niveo.Parser
  ( Expr (..),
    Prog (..),
    Token (..),
    TokenType (..),
    parse,
    program,
  )
import Niveo.Utils (sepByComma)
import Optics (Ixed (ix), at, (%), (%~), (.~), (^.))
import Relude.Extra (toFst, traverseBoth)
import Witch
import Prelude hiding (ask, asks, local, readFile)

evalTxt :: EvalEs :>> es => Eff es Val
evalTxt = do
  ctx <- ask @Context
  prog <- parse program ctx.fin ctx.src & either throwError pure
  eval prog.expr

eval :: EvalEs :>> es => Expr -> Eff es Val
eval expr@(EUnary {op, rhs}) = do
  rhs' <- eval rhs
  case (op.type_, rhs') of
    (TBang, VBool b) -> pure . VBool $ not b
    (TPlus, x@(VNum _)) -> pure x
    (TMinus, VNum x) -> pure . VNum $ negate x
    _ ->
      throwReport
        "mismatched types"
        [(expr.range, This [i|Could not apply `#{op}` to `#{rhs'}`|])]
eval expr@(EBinary {lhs, op, rhs}) = do
  lhs' <- eval lhs
  case (op.type_, lhs') of
    -- Handle short-circuiting for `&&` and `||`.
    (TAmp2, VBool False) -> pure $ VBool False
    (TPipe2, VBool True) -> pure $ VBool True
    _ -> do
      rhs' <- eval rhs
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
        (TAmp2, _, b@(VBool _)) -> pure b
        (TPipe2, _, b@(VBool _)) -> pure b
        _ ->
          throwReport
            "mismatched types"
            [(expr.range, This [i|could not apply `#{op}` to `(#{lhs'}, #{rhs'})`|])]
eval (ECall {callee, args}) =
  (callee, args) & bitraverse eval (eval `traverse`) >>= \case
    (VLambda params body env, args') ->
      if length params == length args
        then
          let localEnv = env & #dict %~ (Map.union . Map.fromList $ (params <&> (.lexeme)) `zip` args')
           in eval body & local @Context (#env .~ localEnv)
        else
          throwReport
            "mismatched types"
            [(callee.range, This [i|invalid args when calling `#{callee}`: expected `(#{sepByComma params})` , found `(#{sepByComma args'})`|])]
    (VHostFun (HostFun {fun}), args') -> fun args'
    _ -> throwReport "invalid call" [(callee.range, This [i|`#{callee}` is not callable|])]
eval (EIndex {this, idx}) =
  let expected (ty :: String) n = throwReport "mismatched types" [(idx.range, This [i|expected #{ty}, found `#{n}`|])]
      noEntry idx' = throwReport "no entry found" [(idx.range, This [i|for key `#{idx'}`|])]
      noIndex len n' = throwReport "index out of bounds" [(idx.range, This [i|the len is #{len} but the index is `#{n'}`|])]
      findKV keyP kvs = kvs & find (keyP . fst) & maybe (noEntry idx) (pure . snd)
   in traverseBoth eval (this, idx) >>= \case
        (VList l, VNum n) ->
          tryInto @Int n
            & either
              (const $ expected "integer" n)
              (\n' -> l Seq.!? n' & maybe (noIndex (Seq.length l) n') pure)
        (VStruct kvs, VStr s) -> kvs & findKV (== NStr s)
        (VStruct kvs, VAtom s) -> kvs & findKV (== NAtom s)
        _ ->
          throwReport
            "mismatched types"
            [(idx.range, This [i|could not apply `[]` to `(#{this}, #{idx})`|])]
eval (EParen {inner}) = eval inner
eval (EList {exprs}) = VList . from <$> eval `traverse` exprs
eval (EIfElse {cond, then_, else_}) =
  eval cond >>= \case
    (VBool cond') -> eval $ if cond' then then_ else else_
    cond' -> throwReport "mismatched types" [(cond.range, This [i|expected boolean condition, found `#{cond'}`|])]
eval (ELet {kw, defs, val}) =
  -- Following the French CAML tradition here: https://stackoverflow.com/a/1891573
  if kw.type_ == TLetrec
    then do
      let define (defs' :: NonEmpty (Token, Val)) =
            let defs'' = Map.fromList $ first (.lexeme) <$> into @[(Token, Val)] defs'
             in #env % #dict %~ Map.union defs''
      defs' <- mfix \defs' ->
        defs & traverse \(ident', def') ->
          eval def' & local @Context (define defs') <&> (ident',)
      eval val & local @Context (define defs')
    else do
      ctx <- ask @Context
      let ctxUpdate ctx' (ident', def') = do
            def'' <- eval def' & local (const ctx')
            ctx' & #env % #dict %~ Map.insert ident'.lexeme def'' & pure
      ctx' <- defs & foldM ctxUpdate ctx
      eval val & local (const ctx')
eval (ELambda {params, body}) = asks @Context $ VLambda params body . (.env)
eval (EStruct {kvs}) = VStruct . from <$> bitraverse evalName eval `traverse` kvs
  where
    evalName expr' =
      eval expr'
        <&> tryInto @Name
        >>= either
          ( \(TryFromException val _) ->
              throwReport
                "mismatched types"
                [(expr'.range, This [i|expected string or atom, found `#{val}`|])]
          )
          pure
eval (ELit l) = pure $ from l
eval (EVar tk) =
  asks @Context (^. #env % #dict % at tk.lexeme)
    >>= maybe
      ( throwReport
          "undefined variable"
          [(tk.range, This [i|`#{tk}` not found in this scope|])]
      )
      pure
eval expr@(EError tk) =
  throwReport
    "internal error"
    [(expr.range, This [i|illegal expression `#{tk}`|])]

-- * The Niveo Prelude

instance Default Env where
  def = prelude

prelude :: Env
prelude = Env {dict = prelude'}
  where
    prelude' =
      Map.fromList $
        second VHostFun . toFst (.name)
          <$> [ HostFun "import" import_,
                HostFun "prepend" prepend,
                HostFun "delete" delete,
                HostFun "rename" rename,
                HostFun "update" update
              ]

unexpectedArgs :: EvalEs :>> es => Text -> Text -> [Val] -> Eff es Val
unexpectedArgs funName expectedTys vs =
  throwReport
    [i|unexpected args when calling `#{funName}`: expected `(#{expectedTys})`, found `(#{sepByComma vs})`|]
    []

import_ :: RawHostFun
import_ [VStr fin] = do
  src <- readFile (into fin)
  let ctx = Context {env = def, fin = into fin, src = into src}
  evalTxt & local (const ctx)
-- Atom for outside of prelude.
-- TODO: Support atoms.
import_ vs = unexpectedArgs "import" "name" vs

prepend :: RawHostFun
prepend vs =
  let abort = unexpectedArgs "prepend" "struct, name, _" vs
   in case vs of
        [VStruct kvs, k, v] ->
          tryInto @Name k
            & either (const abort) (pure . VStruct . (Seq.:<| kvs) . (,v))
        _ -> abort

structFindIndex :: Eq k => k -> Seq (k, v) -> Maybe Int
structFindIndex k = Seq.findIndexL $ (== k) . fst

delete :: RawHostFun
delete vs =
  let abort = unexpectedArgs "delete" "struct, name" vs
      noEntry s idx = throwReport [i|no entry found for key `#{idx}` in `#{s}`|] []
   in case vs of
        [s@(VStruct kvs), k] ->
          tryInto @Name k
            & either
              (const abort)
              ( maybe (noEntry s k) (pure . VStruct . (`Seq.deleteAt` kvs))
                  . (`structFindIndex` kvs)
              )
        _ -> abort

rename :: RawHostFun
rename vs =
  let abort = unexpectedArgs "rename" "struct, name, name" vs
      noEntry s idx = throwReport [i|no entry found for key `#{idx}` in `#{s}`|] []
   in case vs of
        [s@(VStruct kvs), k, k1] ->
          tryInto @Name
            `traverseBoth` (k, k1)
            & either
              (const abort)
              ( \(k', k1') ->
                  kvs
                    & structFindIndex k'
                    & maybe
                      (noEntry s k)
                      (\idx -> pure . VStruct $ kvs & ix idx %~ first (const k1'))
              )
        _ -> abort

update :: RawHostFun
update vs =
  let abort = unexpectedArgs "update" "struct, name, _" vs
      noEntry s idx = throwReport [i|no entry found for key `#{idx}` in `#{s}`|] []
   in case vs of
        [s@(VStruct kvs), k, v] ->
          tryInto @Name k
            & either
              (const abort)
              ( maybe
                  (noEntry s k)
                  (\idx -> pure . VStruct $ kvs & ix idx %~ second (const v))
                  . (`structFindIndex` kvs)
              )
        _ -> abort
