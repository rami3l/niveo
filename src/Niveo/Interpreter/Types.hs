module Niveo.Interpreter.Types
  ( Val (..),
    Name (..),
    Env (..),
    Context (..),
    EvalEs,
    RawHostFun,
    HostFun (..),
  )
where

import Data.Char (toLower)
import Data.String.Interpolate
import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static
import Error.Diagnose.Diagnostic (Diagnostic)
import GHC.Show (Show (..))
import Niveo.Instances ()
import Niveo.Interpreter.FileSystem (FileSystem)
import Niveo.Parser
  ( Expr (..),
    Lit (..),
    LitType (..),
    Token (..),
    TokenType (..),
  )
import Niveo.Utils (sepByComma, showRealFrac)
import Optics.TH (makeFieldLabelsNoPrefix)
import Relude.Unsafe (read)
import Witch
import Prelude hiding (Reader, show)

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
  show (VList vs) = [i|[#{sepByComma (toList vs)}]|]
  show (VStruct kvs) = [i|struct{#{intercalate ", " (toList kvs')}}|] where kvs' = kvs <&> (\(k, v) -> [i|#{k} = #{v}|] :: String)
  show (VAtom s) = '\'' : toString s
  show (VLambda {params}) = [i|<fun(#{sepByComma params'})>|] where params' = params <&> (.lexeme)
  show (VHostFun (HostFun {name})) = [i|<extern fun #{name}>|]
  showList vs = (<>) [i|[#{sepByComma vs}]|]

data Name
  = NStr !Text
  | NAtom !Text
  deriving (Eq)

instance TryFrom Val Name where
  tryFrom (VStr s) = Right $ NStr s
  tryFrom (VAtom s) = Right $ NAtom s
  tryFrom v = Left $ TryFromException v Nothing

instance Show Name where
  show (NStr s) = show s
  show (NAtom s) = '\'' : toString s
  showList ns = (<>) [i|[#{intercalate ", " $ fmap show ns}]|]

newtype Env = Env {dict :: Map Text Val} deriving (Eq, Show)

data Context = Context
  { env :: Env,
    fin :: FilePath,
    src :: Text
  }
  deriving (Generic)

type EvalEs =
  [ Error (Diagnostic Text),
    FileSystem,
    Reader Context
  ]

type RawHostFun = forall es. EvalEs :>> es => [Val] -> Eff es Val

data HostFun = HostFun
  { name :: !Text,
    -- It seems that `hostFun.fun`'s use of `PolymorphicComponents` doesn't work very well with `OverloadedRecordDot`'s `HasField` class.
    -- Try `(HostFun {fun})` instead to access this field.
    --
    -- See: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/hasfield.html>:
    -- > If a record field has a polymorphic type [..] the corresponding HasField constraint will not be solved...
    fun :: RawHostFun
  }

instance Eq HostFun where
  (==) = (==) `on` (.name)

-- Template Haskell makes the order of declarations very important:
-- Mutual referencing declarations can happen exclusively before/after those TH lines,
-- and those using the generated instances can only be placed after them.
-- See: <https://stackoverflow.com/a/20877030>.
makeFieldLabelsNoPrefix ''Env
makeFieldLabelsNoPrefix ''Context
