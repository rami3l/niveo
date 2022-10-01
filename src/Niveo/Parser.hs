module Niveo.Parser
  ( Expr (..),
    Lit (..),
    LitType (..),
    Parser,
    ParserErrorBundle,
    Prog (..),
    Token (..),
    TokenType (..),
    expression,
    parse,
    primary,
    program,
  )
where

import Data.Bimap (Bimap)
import Data.Bimap qualified as Bimap
import Data.Char (isDigit, toLower)
import Data.Either.Extra (mapLeft)
import Data.Map.Strict qualified as Map
import Data.String.Interpolate
import Data.Text qualified as Text
import Data.Tuple.Extra (both)
import Error.Diagnose (Diagnostic, Position (..))
import Error.Diagnose.Compat.Megaparsec (errorDiagnosticFromBundle)
import Error.Diagnose.Diagnostic (addFile)
import GHC.Records (HasField (..))
import GHC.Show (Show (..))
import Niveo.Instances ()
import Optics (makeFieldLabelsNoPrefix)
import Relude.Base
import Text.Megaparsec
  ( MonadParsec (eof, hidden, label, lookAhead, notFollowedBy, takeWhileP, try, withRecovery),
    ParseErrorBundle,
    Parsec,
    SourcePos,
    anySingle,
    between,
    choice,
    getSourcePos,
    manyTill,
    manyTill_,
    option,
    registerParseError,
    satisfy,
    sepBy,
    sepEndBy,
    single,
    (<?>),
  )
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    letterChar,
    space1,
    string,
  )
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Pos (SourcePos (..))
import Prelude hiding (show)

kws :: Bimap TokenType Text
kws =
  fromList
    [ (TTrue, "true"),
      (TFalse, "false"),
      (TNull, "null"),
      (TStruct, "struct"),
      (TIf, "if"),
      (TElse, "else"),
      (TLet, "let"),
      (TFun, "fun")
    ]

ops :: Bimap TokenType Text
ops =
  fromList
    [ (TLParen, "("),
      (TRParen, ")"),
      (TLBrack, "["),
      (TRBrack, "]"),
      (TLBrace, "{"),
      (TRBrace, "}"),
      (TComma, ","),
      (TDot, "."),
      (TSemi, ";"),
      (TColon2, "::"),
      (TColon, ":"),
      (TPlus, "+"),
      (TMinus, "-"),
      (TStar2, "**"),
      (TStar, "*"),
      (TSlash, "/"),
      (TLtEq, "<="),
      (TLt, "<"),
      (TEq2, "=="),
      (TEq, "="),
      (TGtEq, ">="),
      (TGt, ">"),
      (TBangEq, "!="),
      (TBang, "!"),
      (TAmp2, "&&"),
      (TPipe2, "||")
    ]

data TokenType
  = TTrue
  | TFalse
  | TNull
  | TStruct
  | TIf
  | TElse
  | TLet
  | TFun
  | TLParen
  | TRParen
  | TLBrack
  | TRBrack
  | TLBrace
  | TRBrace
  | TComma
  | TDot
  | TSemi
  | TColon
  | TColon2
  | TPlus
  | TMinus
  | TStar
  | TStar2
  | TSlash
  | TLt
  | TLtEq
  | TEq
  | TEq2
  | TGt
  | TGtEq
  | TBang
  | TBangEq
  | TAmp2
  | TPipe2
  | TNum
  | TStr
  | TIdent
  | TAtom
  deriving (Show, Ord, Eq)

data Token = Token
  { type_ :: !TokenType,
    lexeme :: !Text,
    pos :: !SourcePos
  }
  deriving (Eq)

makeFieldLabelsNoPrefix ''Token

instance Show Token where show = toString . (.lexeme)

instance HasField "range" Token Error.Diagnose.Position where
  getField tk =
    let start = both (fromIntegral . Megaparsec.unPos) (sourceLine tk.pos, sourceColumn tk.pos)
        -- Length offset: "lexeme" (+2), 'lexeme (+1), lexeme (0)
        offset = case tk.type_ of TStr -> 2; TAtom -> 1; _ -> 0
        end = second (+ (Text.length tk.lexeme + offset)) start
     in Position start end $ sourceName tk.pos

type ParserError = Void

type ParserStream = Text

type Parser :: Type -> Type
type Parser = Parsec ParserError ParserStream

type ParserErrorBundle = ParseErrorBundle ParserStream ParserError

space :: Parser ()
space =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockCommentNested "/*" "*/")

symbol :: Text -> Parser Text
symbol = L.symbol space

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

-- | Returns a 'Token' parser given a 'Text' parser to be mapped from and the expected 'TokenType'.
toTokenParser :: TokenType -> Parser Text -> Parser Token
toTokenParser tkType str = do
  pos <- getSourcePos
  str' <- str
  pure $ Token tkType str' pos

-- | A reserved keyword in the language.
rword :: TokenType -> Text -> Parser Token
rword tkType str = toTokenParser tkType rword'
  where
    rword' = lexeme . try $ string str <* notFollowedBy alphaNumChar

-- | A reserved symbol in the language.
rsym :: TokenType -> Text -> Parser Token
rsym tkType = toTokenParser tkType . symbol

kwMap, opMap :: Map TokenType (Parser Token)
kwMap = kws & Bimap.toMap & Map.mapWithKey rword
opMap = ops & Bimap.toMap & Map.mapWithKey rsym

kw, op :: TokenType -> Parser Token
kw = (kwMap Map.!)
op = (opMap Map.!)

ident, atom :: Parser Token
ident = toTokenParser TIdent ident' <?> "identifier"
  where
    ident' = lexeme . try $ check . toText =<< identStr
    check str =
      if str `Bimap.memberR` kws
        then fail [i|keyword `#{str}` cannot be an identifier|]
        else pure str
atom = toTokenParser TAtom atom' <?> "atom"
  where
    atom' = lexeme . try $ toText <$> (char '\'' *> identStr)

identStr :: Parser String
identStr = (:) <$> (letterChar <|> single '_') <*> (hidden . many) (alphaNumChar <|> single '_')

strLit :: Parser Token
strLit = toTokenParser TStr strLit' <?> "string literal"
  where
    strLit' = lexeme $ toText <$> (doubleQuote *> L.charLiteral `manyTill` doubleQuote)
    doubleQuote = char '"'

numLit :: Parser Token
numLit = label "number literal" $ lexeme do
  pos <- getSourcePos
  car <- digits
  Token TNum car pos `option` hidden do
    cdr <- char '.' *> digits
    pure $ Token TNum [i|#{car}.#{cdr}|] pos

digits :: Parser Text
digits = Text.cons <$> (satisfy isDigit <?> "digit") <*> hidden (takeWhileP Nothing isDigit)

data LitType = LNull | LBool | LNum | LStr | LAtom deriving (Eq)

data Lit = Lit {type_ :: !LitType, tk :: !Token} deriving (Eq)

instance HasField "range" Lit Error.Diagnose.Position where
  getField (Lit _ tk) = tk.range

instance Show Lit where
  show (Lit LNull _) = "null"
  show (Lit LBool b) = toLower <$> toString b.lexeme
  show (Lit LNum n) = toString n.lexeme
  show (Lit LStr s) = show s.lexeme
  show (Lit LAtom s) = '\'' : toString s.lexeme

data Expr
  = EUnary {op :: !Token, rhs :: Expr}
  | EBinary {lhs :: Expr, op :: !Token, rhs :: Expr}
  | ECall {callee :: Expr, args :: [Expr], end :: !Token}
  | EIndex {this, idx :: Expr, end :: !Token}
  | EParen {inner :: Expr, end :: !Token}
  | EList {exprs :: [Expr], end :: !Token}
  | EIfElse {kw :: !Token, cond, then', else' :: Expr}
  | ELet {ident :: !Token, def, val :: Expr}
  | ELambda {kw :: !Token, params :: ![Token], body :: Expr}
  | EStruct {kw :: !Token, kvs :: [(Expr, Expr)]}
  | ELit !Lit
  | EVar !Token
  | EError !Token
  deriving (Eq)

instance HasField "range" Expr Error.Diagnose.Position where
  getField (EUnary op' _) = op'.range
  getField (EBinary _ op' _) = op'.range
  getField (ECall _ _ end') = end'.range
  getField (EIndex _ _ end') = end'.range
  getField (EParen _ end') = end'.range
  getField (EList _ end') = end'.range
  getField (EIfElse kw' _ _ _) = kw'.range
  getField (ELet ident' _ _) = ident'.range
  getField (ELambda kw' _ _) = kw'.range
  getField (EStruct kw' _) = kw'.range
  getField (ELit lit) = lit.range
  getField (EVar var) = var.range
  getField (EError tk) = tk.range

instance Show Expr where
  show (EUnary op' rhs) = [i|(#{op'} #{rhs})|]
  show (EBinary lhs op' rhs) = [i|(#{op'} #{lhs} #{rhs})|]
  show (ECall callee args _) = show $ Showable callee : Showable `fmap` args
  show (EIndex this idx _) = [i|(@ #{this} #{idx})|]
  show (EParen inner _) = show inner
  show (EList exprs _) = show $ Showable (ToString' @Text "list") : Showable `fmap` exprs
  show (EIfElse _ cond then' else') = [i|(if #{cond} #{then'} #{else'})|]
  show (ELet ident' def' val) = [i|(let ((#{ident'} #{def'})) #{val})|]
  show (ELambda _ params body) = [i|(lambda #{show params'} #{body})|] where params' = Showable . ToString' . (.lexeme) <$> params
  show (EStruct _ kvs) = [i|(struct #{show kvs'})|] where kvs' = kvs <&> \case (k, v) -> Showable [Showable k, Showable v]
  show (ELit lit) = show lit
  show (EVar var) = var.lexeme & toString
  show (EError tk) = [i|"<error #{tk}>"|]

newtype Prog = Prog {expr :: Expr}

instance Show Prog where
  show (Prog val) = show val

data Showable = forall a. Show a => Showable a

instance Show Showable where
  show (Showable a) = show a
  showList [] = const "'()"
  showList as = const [i|(#{intercalate " " $ fmap show as})|]

data ToString' = forall a. ToString a => ToString' a

instance Show ToString' where
  show (ToString' a) = toString a

-- Expressions:

block :: Parser Expr
block = EParen <$> (op TLBrace *> expression) <*> op TRBrace <?> "block"

paramList :: Parser [Token]
paramList = between (op TLParen) (op TRParen) (ident `sepBy` hidden (op TComma)) <?> "parameters"

primary :: Parser Expr
primary =
  choice
    [ kw TTrue <&> ELit . Lit LBool,
      kw TFalse <&> ELit . Lit LBool,
      kw TNull <&> ELit . Lit LNull,
      numLit <&> ELit . Lit LNum,
      strLit <&> ELit . Lit LStr,
      EParen <$> (op TLParen *> expression) <*> op TRParen,
      block,
      -- TODO: Remove EBlock and add ELet in the reference manual.
      ELet
        <$> (kw TLet *> ident)
        <*> (op TEq *> expression)
        <*> (op TSemi *> expression),
      EList <$> (op TLBrack *> (expression `sepEndBy` op TComma)) <*> op TRBrack,
      EStruct <$> kw TStruct <*> between (op TLBrace) (op TRBrace) (structKV `sepEndBy` op TComma),
      EIfElse
        <$> kw TIf
        <*> between (op TLParen) (op TRParen) (expression <?> "condition")
        <*> (expression <?> "then branch")
        <*> (kw TElse *> (expression <?> "else branch")),
      ELambda <$> kw TFun <*> paramList <*> (block <?> "function body"),
      atom <&> ELit . Lit LAtom,
      EVar <$> ident
    ]
    <?> "primary expression"
  where
    structKV =
      ( do
          -- Sugar when the key string can be parsed as ident.
          -- `foo: bar` => `"foo" = bar`
          -- `foo` => `"foo" = foo` (Named field punning)
          field <- ident <?> "field"
          let fieldStr = ELit . Lit LStr $ field
          value <- optional (op TColon *> (expression <?> "value"))
          pure (fieldStr, value & fromMaybe (EVar field))
      )
        <|> (,) <$> (expression <?> "field") <*> (op TEq *> (expression <?> "value"))

toInfixLParser :: Parser Expr -> (Expr -> Parser Expr) -> Parser Expr
toInfixLParser car cdr = do
  c <- car <?> "operand"
  c `option` go c
  where
    go c = hidden $ do
      c' <- cdr c
      c' `option` go c'

call :: Parser Expr
call = label "call expression" $
  toInfixLParser primary \c -> choice $ [goArgs, goGet, goIndex] <&> ($ c)
  where
    goArgs c = ECall c <$> (op TLParen *> args) <*> op TRParen
    args = expression `sepBy` hidden (op TComma) <?> "arguments"
    goGet c = do
      -- Sugar in indexing when the key string/atom can be parsed as ident.
      -- `this.prop` => `this["prop"]`
      -- `this::prop` => `this['prop]`
      (op', rawField) <- (,) <$> (op TDot <|> op TColon2) <*> (ident <?> "field")
      let field = ELit . Lit (if op'.type_ == TDot then LStr else LAtom) $ rawField
      pure $ EIndex c field rawField
    goIndex c = EIndex c <$> (op TLBrack *> (expression <?> "index")) <*> op TRBrack

unary, pow, factor, term, comparison, equality, logicAnd, logicOr :: Parser Expr
unary =
  (EUnary <$> choice (op <$> [TBang, TPlus, TMinus]) <*> (unary <?> "operand"))
    <|> call
pow = do
  lhs <- unary <?> "operand"
  lhs `option` hidden (EBinary lhs <$> op TStar2 <*> pow)
factor = toInfixLParser pow \c ->
  EBinary c <$> (op TSlash <|> op TStar) <*> pow
term = toInfixLParser factor \c ->
  EBinary c <$> (op TMinus <|> op TPlus) <*> factor
comparison = toInfixLParser term \c ->
  EBinary c
    <$> (op TGtEq <|> op TGt <|> op TLtEq <|> op TLt)
    <*> term
equality = toInfixLParser comparison \c ->
  EBinary c <$> (op TBangEq <|> op TEq2) <*> comparison
logicAnd = toInfixLParser equality \c ->
  EBinary c <$> op TAmp2 <*> equality
logicOr = toInfixLParser logicAnd \c ->
  EBinary c <$> op TPipe2 <*> logicAnd

expression :: Parser Expr
expression = synced logicOr <?> "expression"
  where
    synced = withRecovery $ (*> (EError <$> sync end)) . registerParseError
    end = choice $ op TSemi : (lookAhead . kw <$> [TTrue, TFalse, TNull, TStruct, TIf, TFun])

-- | Syncs the parser towards the given Token pattern parser.
--
-- For example, in a C statement,
-- this pattern should match at the end of the current statement,
-- or right before the next one.
--
-- See: <https://craftinginterpreters.com/parsing-expressions.html#synchronizing-a-recursive-descent-parser>
sync :: Parser Token -> Parser Token
sync = (snd <$>) . (anySingle `manyTill_`)

program :: Parser Prog
program = expression <* eof <&> Prog <?> "program"

-- | A version of `Megaparsec.parse` supercharged by `diagnose` errors.
parse ::
  -- | The parser to be run.
  Parser a ->
  -- | The input file path.
  FilePath ->
  -- | The input text.
  Text ->
  Either (Diagnostic Text) a
parse parser fin src =
  Megaparsec.parse parser fin src & mapLeft \bundle ->
    bundle
      & errorDiagnosticFromBundle Nothing "Parse error on input" Nothing
      & \diag -> diag `addFile` fin $ toString src
