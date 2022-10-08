module Niveo.Interpreter
  ( Val (..),
    Env,
    Context (..),
    interpret,
    interpret',
    eval,
  )
where

import Control.Monad (foldM)
import Control.Monad.Fix (mfix)
import Data.Char (toLower)
import Data.Default (Default (def))
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.String.Interpolate
import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static
import Error.Diagnose (Marker (This), addFile, addReport, err)
import Error.Diagnose.Diagnostic (Diagnostic)
import Error.Diagnose.Position (Position)
import GHC.Show (Show (..))
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
import Optics (at, (%), (%~), (.~), (^.))
import Optics.TH (makeFieldLabelsNoPrefix)
import Relude.Extra (toFst, traverseBoth)
import Relude.Unsafe (read)
import Witch
import Prelude hiding (Reader, ask, asks, local, runReader, show, withReader)

data Val
  = -- Native JSON types.
    VNull
  | VBool !Bool
  | VNum !Double
  | VStr !Text
  | VList (Seq Val)
  | -- | `VStruct` is encoded as a list of @(tag, value)@ pairs, and the `tag`s can be identical,
    -- in which case only the `value` of the *leftmost* `tag` is retrieved.
    VStruct (Seq (Name, Val))
  | -- Extra types.
    VAtom !Text
  | VLambda {params :: ![Token], body :: Expr, env :: Env}
  | VHostFun HostFun
  deriving (Eq)

instance From Lit Val where
  from (Lit LNull _) = VNull
  from (Lit LBool b) = VBool $ b.type_ == TTrue
  from (Lit LNum x) = VNum . read . into $ x.lexeme
  from (Lit LStr s) = VStr s.lexeme
  from (Lit LAtom s) = VAtom s.lexeme

instance Show Val where
  show VNull = "null"
  show (VBool b) = toLower <$> show b
  show (VNum n) = showRealFrac n
  show (VStr s) = show s
  show (VList vs) = [i|[#{intercalate ", " vs'}]|] where vs' = vs <&> show & toList
  show (VStruct kvs) = [i|struct{#{intercalate ", " kvs'}}|] where kvs' = kvs <&> (\(k, v) -> [i|#{k} = #{v}|]) & toList
  show (VAtom s) = '\'' : toString s
  show (VLambda params _ _) = [i|<fun(#{intercalate ", " params'})>|] where params' = into . (.lexeme) <$> params
  show (VHostFun (HostFun name _)) = [i|<extern fun #{name}>|]
  showList vs = (<>) [i|[#{intercalate ", " $ fmap show vs}]|]

-- | If `n` is an `Integer`, then show it as an integer.
-- Otherwise it will be shown normally.
showRealFrac :: Double -> String
showRealFrac n = n & tryInto @Integer & either (const $ show n) show

data Name = NStr !Text | NAtom !Text deriving (Eq)

instance TryFrom Val Name where
  tryFrom (VStr s) = Right $ NStr s
  tryFrom (VAtom s) = Right $ NAtom s
  tryFrom v = Left $ TryFromException v Nothing

instance Show Name where
  show (NStr s) = show s
  show (NAtom s) = '\'' : toString s
  showList ns = (<>) [i|[#{intercalate ", " $ fmap show ns}]|]

newtype Env = Env {dict :: Map Text Val} deriving (Eq, Show)

instance Default Env where
  def = Env {dict}
    where
      dict =
        Map.fromList $
          second VHostFun . toFst (.name)
            <$> [HostFun "import" import_]

import_ :: RawHostFun
import_ [VStr path] = undefined
-- Atom for outside of prelude.
import_ _ = undefined

data Context = Context
  { env :: Env,
    fin :: FilePath,
    src :: Text
  }
  deriving (Generic)

type EvalEffs = [Error (Diagnostic Text), Reader Context]

type RawHostFun = [Val] -> Eff EvalEffs Val

data HostFun = HostFun
  { name :: !Text,
    fun :: RawHostFun
  }

instance Eq HostFun where
  hf == hf' = hf.name == hf'.name

makeFieldLabelsNoPrefix ''Env
makeFieldLabelsNoPrefix ''Context

interpret' :: Context -> Either (Diagnostic Text) Val
interpret' ctx = runPureEff . runReader ctx . runErrorNoCallStack $ interpret

interpret :: Eff EvalEffs Val
interpret = do
  ctx <- ask @Context
  prog <- parse program ctx.fin ctx.src & either throwError pure
  eval prog.expr

eval :: EvalEffs :>> es => Expr -> Eff es Val
eval expr@(EUnary op rhs) = do
  rhs' <- eval rhs
  case (op.type_, rhs') of
    (TBang, VBool b) -> pure . VBool $ not b
    (TPlus, VNum x) -> pure . VNum $ x
    (TMinus, VNum x) -> pure . VNum $ negate x
    _ ->
      throwReport
        "mismatched types"
        [(expr.range, This [i|Could not apply `#{op}` to `#{rhs'}`|])]
eval expr@(EBinary lhs op rhs) = do
  -- Lazy evaluation! No need to worry about short-circuiting.
  (lhs', rhs') <- traverseBoth eval (lhs, rhs)
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
eval (ECall callee args _) =
  (callee, args) & bitraverse eval (eval `traverse`) >>= \case
    (VLambda params body env, args') ->
      let localEnv = env & #dict %~ (Map.union . Map.fromList $ (params <&> (.lexeme)) `zip` args')
       in eval body & local @Context (#env .~ localEnv)
    (VHostFun hf, args') -> hf.fun args' & inject
    _ -> throwReport "invalid call" [(callee.range, This [i|`#{callee}` is not callable|])]
eval (EIndex this idx _) =
  let expected (ty :: String) n = throwReport "mismatched types" [(idx.range, This [i|expected #{ty}, found `#{n}`|])]
      noEntry idx' = throwReport "no entry found" [(idx.range, This [i|for key `#{idx'}`|])]
      findKV keyP kvs = kvs & find (keyP . fst) & maybe (noEntry idx) (pure . snd)
   in traverseBoth eval (this, idx) >>= \case
        (VList l, VNum n) ->
          tryInto @Int n
            & either
              (const $ expected "integer" n)
              ( \n' ->
                  l Seq.!? n'
                    & maybe
                      (throwReport "index out of bounds" [(idx.range, This [i|the len is #{Seq.length l} but the index is `#{n'}`|])])
                      pure
              )
        (VStruct kvs, VStr s) -> kvs & findKV (== NStr s)
        (VStruct kvs, VAtom s) -> kvs & findKV (== NAtom s)
        _ ->
          throwReport
            "mismatched types"
            [(idx.range, This [i|could not apply `[]` to `(#{this}, #{idx})`|])]
eval (EParen x _) = eval x
eval (EList xs _) = VList . from <$> eval `traverse` xs
eval (EIfElse _ cond then_ else_) =
  eval cond >>= \case
    (VBool cond') -> eval $ if cond' then then_ else else_
    cond' -> throwReport "mismatched types" [(cond.range, This [i|expected boolean condition, found `#{cond'}`|])]
eval (ELet kw defs val) =
  do
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
              def'' <- eval def' & runReader ctx'
              ctx' & #env % #dict %~ Map.insert ident'.lexeme def'' & pure
        ctx' <- defs & foldM ctxUpdate ctx
        eval val & runReader ctx'
eval (ELambda _ params body) = asks @Context $ VLambda params body . (.env)
eval (EStruct _ kvs) = VStruct . from <$> bitraverse evalName eval `traverse` kvs
  where
    evalName expr' =
      eval expr' <&> tryInto @Name >>= \case
        Right name -> pure name
        Left (TryFromException val _) ->
          throwReport
            "mismatched types"
            [(expr'.range, This [i|expected string or atom, found `#{val}`|])]
eval (ELit l) = pure $ from l
eval (EVar tk) =
  ask @Context
    <&> (^. #env % #dict % at tk.lexeme)
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

throwReport ::
  [Error (Diagnostic Text), Reader Context] :>> es =>
  Text ->
  [(Position, Marker Text)] ->
  Eff es a
throwReport msg markers = do
  ctx <- ask @Context
  let diag = addFile @Text def ctx.fin $ toString ctx.src
  throwError . (diag `addReport`) $ err Nothing msg markers []
