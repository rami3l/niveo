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
import Data.Aeson qualified as Aeson
import Data.Default (Default (def))
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.String.Interpolate
import Data.Text qualified as Text
import Data.Tuple.Extra (secondM)
import Data.Tuple.Optics
import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static
import Error.Diagnose (Marker (This))
import Error.Diagnose.Position (Position)
import Niveo.Instances ()
import Niveo.Interpreter.FileSystem (readFile)
import Niveo.Interpreter.Std (stdMod)
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
import Optics (Ixed (ix), at, (%))
import Optics.Operators
import Optics.Operators.Unsafe
import Relude.Extra (toFst, traverseBoth)
import System.FilePath (hasExtension, normalise, takeDirectory, (</>))
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
    _ -> throwReport "mismatched types" [(expr.range, This [i|Could not apply `#{op}` to `#{rhs'}`|])]
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
          throwReport "mismatched types" [(expr.range, This [i|could not apply `#{op}` to `(#{lhs'}, #{rhs'})`|])]
eval (ECall {callee, args}) = do
  let margs = eval `traverse` args
  eval callee >>= \case
    callee'@(VLambda params body ctx)
      | let (paramCount, argCount) = (length params, length args),
        paramCount /= argCount ->
          throwReport "invalid call" [(callee.range, This [i|`#{callee'}` expects #{paramCount} arguments, found #{argCount}|])]
      | otherwise -> do
          argBinds <- Map.fromList . zip (params <&> (.lexeme)) <$> margs
          -- `Map.union` is a left-biased union, which means that the bindings in the inner scope
          -- will shadow those in the captured scope.
          let localEnv = ctx.env & #dict %~ Map.union argBinds
          eval body & local @Context (const ctx {env = localEnv})
    VHostFun (HostFun {fun}) -> fun callee.range =<< margs
    callee' -> throwReport "invalid call" [(callee.range, This [i|`#{callee'}` is not callable|])]
eval (EIndex {this, idx}) = do
  (this', idx') <- traverseBoth eval (this, idx)
  let index l = \case
        VNum n
          | Right n' <- tryInto @Int n -> l ^? ix n' & maybe (noIndex (Seq.length l) n') pure
          | otherwise -> unexpectedTy idx.range "integer" n
        _ -> abort this' idx'
  let indexS s = \case
        VNum n
          | Right n' <- tryInto @Int n -> into @String s ^? ix n' & maybe (noIndex (Text.length s) n') pure
          | otherwise -> unexpectedTy idx.range "integer" n
        _ -> abort this' idx'
  let kvs `search` k = case tryInto @Name k of
        Right k'
          | Just idx'' <- k' `structFindIndex` kvs -> pure . snd $ kvs ^?! ix idx''
          | otherwise -> noEntry k
        _ -> unexpectedTy idx.range "name" k
  case (this', idx') of
    (VList l, VList is) -> is & traverse (l `index`) <&> VList
    (VList l, i') -> l `index` i'
    (VStr s, VList is) -> is & traverse (s `indexS`) <&> VStr . via @String
    (VStr s, i') -> s `indexS` i' <&> VStr . one
    (VStruct kvs, VList ks) -> ks & traverse (kvs `search`) <&> VList
    (VStruct kvs, k) -> kvs `search` k
    _ -> abort this' idx'
  where
    noEntry idx' = throwReport "no entry found" [(idx.range, This [i|for key `#{idx'}`|])]
    noIndex :: EvalEs :>> es => Int -> Int -> Eff es a
    noIndex len' n' = throwReport "index out of bounds" [(idx.range, This [i|the len is #{len'} but the index is `#{n'}`|])]
    abort :: EvalEs :>> es => Val -> Val -> Eff es a
    abort this' idx' = throwReport "mismatched types" [(idx.range, This [i|could not apply `[]` to `(#{this'}, #{idx'})`|])]
eval (EParen {inner}) = eval inner
eval (EList {exprs}) = VList . from <$> eval `traverse` exprs
eval (EIfElse {cond, then_, else_}) =
  eval cond >>= \case
    (VBool cond') -> eval $ if cond' then then_ else else_
    cond' -> unexpectedTy cond.range "boolean" cond'
eval (ELet {kw, defs, res})
  | kw.type_ == TLetrec = do
      -- Following the French CAML tradition here: https://stackoverflow.com/a/1891573
      defs' <- mfix \defs' -> secondM (local (bind defs') . eval) `traverse` defs
      eval res & local (bind defs')
  | otherwise = do
      ctx <- ask
      ctx' <- defs & foldM ctxUpdate ctx
      eval res & local (const ctx')
  where
    bind :: NonEmpty (Token, Val) -> Context -> Context
    bind defs' = #env % #dict %~ Map.union defs'' where defs'' = Map.fromList . into $ first (.lexeme) <$> defs'
    ctxUpdate ctx' (ident', def') = do
      def'' <- eval def' & local (const ctx')
      pure $ one (ident', def'') `bind` ctx'
eval (ELambda {params, body}) = asks @Context $ VLambda params body
eval (EStruct {kvs}) = VStruct . from <$> bitraverse evalName eval `traverse` kvs
  where
    evalName expr' =
      eval expr'
        <&> tryInto @Name
        >>= either (\(TryFromException val _) -> unexpectedTy expr'.range "name" val) pure
eval (ELit l) = pure $ from l
eval (EVar tk) =
  asks @Context (^. #env % #dict % at tk.lexeme)
    >>= maybe (throwReport "undefined variable" [(tk.range, This [i|`#{tk}` not found in this scope|])]) pure
eval expr@(EError tk) = throwReport "internal error" [(expr.range, This [i|illegal expression `#{tk}`|])]

-- * Common runtime errors

unexpectedArgs :: EvalEs :>> es => Position -> Text -> [Val] -> Eff es a
unexpectedArgs funRange expected vs =
  throwReport
    "mismatched types"
    [(funRange, This [i|unexpected args in this call: expected `#{expected}`, found `(#{sepByComma vs})`|])]

unexpectedTy :: (EvalEs :>> es, Show s) => Position -> Text -> s -> Eff es a
unexpectedTy range tyName v = throwReport "mismatched types" [(range, This [i|expected #{tyName}, found `#{show @String v}`|])]

-- * The Niveo prelude

instance Default Env where
  def = prelude

prelude :: Env
prelude = Env {dict = prelude'}
  where
    prelude' = hostFuns <&> toFst (.name) <&> second VHostFun & into
    hostFuns =
      [ HostFun "delete" delete,
        HostFun "has" has_,
        HostFun "head" head_,
        HostFun "import" import_,
        HostFun "import_json" importJSON,
        HostFun "init" init_,
        HostFun "last" last_,
        HostFun "len" len,
        HostFun "mod" mod_,
        HostFun "prepend" prepend,
        HostFun "range" range_,
        HostFun "rename" rename,
        HostFun "reverse" reverse_,
        HostFun "tail" tail_,
        HostFun "to_string" toString_,
        HostFun "update" update
      ]

import_ :: RawHostFun
import_ range [VStr fin] = do
  -- We assume that a source file is imported if `fin` has an extension.
  ctx <-
    if into fin & hasExtension
      then do
        dir <- asks @Context $ takeDirectory . (.fin)
        let fin' = normalise $ dir </> into fin
        src <- readFile fin'
        pure Context {env = def, fin = fin', src = into src}
      else do
        -- Otherwise, we're importing a module.
        -- As for now, the only available module is std.
        let mod' = stdMod fin
        let invalidModule = throwReport "invalid import" [(range, This [i|`#{fin} doesn't seem to be a valid source or module`|])]
        mod' & maybe invalidModule (\src -> pure Context {env = def, fin = into fin, src})
  evalTxt & local (const ctx)
import_ range vs = unexpectedArgs range "(name)" vs

importJSON :: RawHostFun
importJSON range [VStr fin] = do
  ctx <- ask @Context
  let invalidJSON = throwReport "invalid import" [(range, This [i|`#{fin}` doesn't seem to be a valid JSON file|])]
  src <- readFile . normalise $ takeDirectory ctx.fin </> into fin
  Aeson.decode @Aeson.Value (via @(UTF_8 LByteString) src) <&> into @Val & maybe invalidJSON pure
importJSON range vs = unexpectedArgs range "(name)" vs

toString_ :: RawHostFun
toString_ _ vs = vs <&> (\case VStr s -> s; v -> show v) & Text.concat & VStr & pure

range_, mod_ :: RawHostFun
range_ range vs = case vs of
  [VNum x, VNum y] -> do
    (x', y') <- traverseBoth (either (const abort) pure . tryInto @Int) (x, y)
    pure . VList . from $ VNum . fromIntegral <$> [x' .. y' - 1]
  _ -> abort
  where
    abort :: EvalEs :>> es => Eff es a
    abort = unexpectedArgs range "(int, int)" vs
mod_ range vs = case vs of
  [VNum x, VNum y] -> do
    (x', y') <- traverseBoth (either (const abort) pure . tryInto @Int) (x, y)
    pure . VNum . fromIntegral $ x' `mod` y'
  _ -> abort
  where
    abort :: EvalEs :>> es => Eff es a
    abort = unexpectedArgs range "(int, int)" vs

len, reverse_ :: RawHostFun
len _ [VList l] = pure . VNum . fromIntegral . length $ l
len _ [VStr s] = pure . VNum . fromIntegral . Text.length $ s
len range vs = unexpectedArgs range "(list|string)" vs
reverse_ _ [VList l] = pure . VList $ Seq.reverse l
reverse_ _ [VStr s] = pure . VStr $ Text.reverse s
reverse_ range vs = unexpectedArgs range "(list|string)" vs

head_, tail_, init_, last_ :: RawHostFun
head_ _ [VList (v Seq.:<| _)] = pure v
head_ range vs = unexpectedArgs range "(non_empty_list)" vs
tail_ _ [VList (_ Seq.:<| vs)] = pure . VList $ vs
tail_ range vs = unexpectedArgs range "(non_empty_list)" vs
init_ _ [VList (vs Seq.:|> _)] = pure . VList $ vs
init_ range vs = unexpectedArgs range "(non_empty_list)" vs
last_ _ [VList (_ Seq.:|> v)] = pure v
last_ range vs = unexpectedArgs range "(non_empty_list)" vs

has_ :: RawHostFun
has_ range vs =
  VBool <$> case vs of
    [VList l, VList is] -> is & traverse (l `index`) <&> and
    [VList l, i'] -> l `index` i'
    [VStr s, VList is] -> is & traverse ((s & into @String) `index`) <&> and
    [VStr s, i'] -> s & into @String & (`index` i')
    [VStruct kvs, VList ks] -> ks & traverse (kvs `search`) <&> and
    [VStruct kvs, k] -> kvs `search` k
    _ -> abort
  where
    abort :: EvalEs :>> es => Eff es a
    abort = unexpectedArgs range "(list, int | list(int)) | (struct, name | list(name))" vs
    index l = \case
      VNum n | Right n' <- tryInto @Int n -> pure $ 0 <= n' && n' < length l
      _ -> abort
    kvs `search` k = tryInto @Name k & either (const abort) (pure . isJust . (`structFindIndex` kvs))

prepend :: RawHostFun
prepend range vs
  | [VStruct kvs, k, v] <- vs, Right k' <- tryInto @Name k = pure . VStruct $ (k', v) Seq.:<| kvs
  | otherwise = unexpectedArgs range "(struct, name, _)" vs

structFindIndex :: Eq k => k -> Seq (k, v) -> Maybe Int
structFindIndex k = Seq.findIndexL $ (== k) . fst

delete :: RawHostFun
delete range vs
  | [s@(VStruct kvs), k] <- vs,
    Right k' <- tryInto @Name k = case k' `structFindIndex` kvs of
      Just idx -> pure . VStruct $ idx `Seq.deleteAt` kvs
      Nothing -> throwReport [i|no entry found for key `#{k}` in `#{s}`|] []
  | otherwise = unexpectedArgs range "(struct, name)" vs

rename :: RawHostFun
rename range vs
  | [s@(VStruct kvs), k, k1] <- vs,
    Right (k', k1') <- tryInto @Name `traverseBoth` (k, k1) = case k' `structFindIndex` kvs of
      Just idx -> pure . VStruct . (ix idx % _1 .~ k1') $ kvs
      Nothing -> throwReport [i|no entry found for key `#{k}` in `#{s}`|] []
  | otherwise = unexpectedArgs range "(struct, name, name)" vs

update :: RawHostFun
update range vs
  | [s@(VStruct kvs), k, v] <- vs,
    Right k' <- tryInto @Name k = case k' `structFindIndex` kvs of
      Just idx -> pure . VStruct . (ix idx % _2 .~ v) $ kvs
      Nothing -> throwReport [i|no entry found for key `#{k}` in `#{s}`|] []
  | otherwise = unexpectedArgs range "(struct, name, _)" vs
