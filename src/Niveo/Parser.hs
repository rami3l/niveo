module Niveo.Parser
  ( Expr,
    Parser,
    ParserErrorBundle,
    Decl,
    Token,
    TokenType,
  )
where

import Data.Bimap (Bimap)
import Data.Bimap qualified as Bimap
import Data.Char (isDigit, toLower)
import Data.Map.Strict qualified as Map
import Data.String.Interpolate
import Data.Text qualified as Text
import Optics (both, toListOf, traversed, (%))
import Relude
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
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    letterChar,
    space1,
    string,
  )
import Text.Megaparsec.Char.Lexer qualified as L
import Prelude qualified

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
  { type_ :: TokenType,
    lexeme :: Text,
    pos :: SourcePos
  }
  deriving (Eq)

instance Prelude.Show Token where show = toString . (.lexeme)

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
    atom' = lexeme . try $ toText <$> (char ':' *> identStr)

identStr :: Parser String
identStr = (:) <$> letterChar <*> (hidden . many) (alphaNumChar <|> single '_')

strLit :: Parser Token
strLit = toTokenParser TStr $ toText <$> (doubleQuote *> L.charLiteral `manyTill` doubleQuote)
  where
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

data Lit
  = LNull
  | LBool !Bool
  | LNum !Text
  | LStr !Text
  | LAtom !Text

instance Prelude.Show Lit where
  show LNull = "null"
  show (LBool b) = toLower <$> show b
  show (LNum n) = show n
  show (LStr s) = show s
  show (LAtom s) = [i|'#{s}|]

data Expr
  = EUnary {op :: !Token, rhs :: Expr}
  | EBinary {lhs :: Expr, op :: !Token, rhs :: Expr}
  | ECall {callee :: Expr, args :: [Expr], end :: !Token}
  | EIndex {from, idx :: Expr, end :: !Token}
  | EParen Expr
  | EBlock {decls :: [Decl], val :: Expr}
  | EList [Expr]
  | EIfElse {cond, then', else' :: Expr}
  | ELambda {params :: ![Token], body :: Expr}
  | EStruct [(Expr, Expr)]
  | ELit !Lit
  | EVar !Token
  | EError

data Decl
  = DLet {ident :: Token, val :: Expr}
  | DFun {ident :: Token, params :: ![Token], body :: Expr}
  | DError

data Prog = Prog {decls :: [Decl], val :: Expr}

instance Prelude.Show Expr where
  show (EUnary op' rhs) = [i|(#{op'} #{rhs})|]
  show (EBinary lhs op' rhs) = [i|(#{op'} #{lhs} #{rhs})|]
  show (ECall callee args _) = show $ Showable callee : Showable `fmap` args
  show (EIndex this idx _) = [i|(@ #{this} #{idx})|]
  show (EParen inner) = show inner
  show (EBlock decls val) = show $ Showable @Text "begin" : Showable `fmap` decls <> [Showable val]
  show (EList exprs) = show $ Showable @Text "list" : Showable `fmap` exprs
  show (EIfElse cond then' else') = [i|(if #{cond} #{then'} #{else'})|]
  show (ELambda params body) = [i|(lambda #{params'} #{body})|] where params' = Showable . (.lexeme) <$> params
  show (EStruct kvs) =
    show $
      Showable @Text "struct"
        -- https://stackoverflow.com/a/14370010
        : Showable
        `fmap` toListOf (traversed % both) kvs
  show (ELit lit) = show lit
  show (EVar var) = var.lexeme & toString
  show EError = "<?>"

instance Prelude.Show Decl where
  show (DLet ident' val) = [i|(let #{ident'} #{val})|]
  show (DFun ident' params body) = [i|(fun #{ident'} #{params'} #{body})|] where params' = Showable . (.lexeme) <$> params
  show DError = "(?)"

instance Prelude.Show Prog where
  show (Prog decls val) = show $ Showable `fmap` decls <> [Showable val]

data Showable = forall a. Show a => Showable a

instance Prelude.Show Showable where
  show (Showable a) = show a
  showList [] = const "'()"
  showList as = const [i|(#{Prelude.unwords $ fmap show as})|]

-- Expressions:

block :: Parser Expr
block = uncurry EBlock <$> block'
  where
    block' = declaration `manyTill_` expression & between (op TLBrace) (op TRBrace) <?> "block"

paramList :: Parser [Token]
paramList = between (op TLParen) (op TRParen) (ident `sepBy` hidden (op TComma)) <?> "parameters"

primary :: Parser Expr
primary =
  choice
    [ kw TTrue $> ELit (LBool True),
      kw TFalse $> ELit (LBool False),
      kw TNull $> ELit LNull,
      numLit <&> ELit . LNum . (.lexeme),
      strLit <&> ELit . LStr . (.lexeme),
      EParen <$> between (op TLParen) (op TRParen) expression,
      block,
      EList <$> between (op TLBrack) (op TRBrack) (expression `sepEndBy` op TComma),
      EStruct <$> between (op TLBrace) (op TRBrace) (structKV `sepEndBy` op TComma),
      EIfElse
        <$> (kw TIf *> between (op TLParen) (op TRParen) (expression <?> "condition"))
        <*> (expression <?> "then branch")
        <*> (kw TElse *> (expression <?> "else branch")),
      ELambda <$> (kw TFun *> paramList) <*> block,
      atom <&> ELit . LAtom . (.lexeme),
      EVar <$> ident
    ]
    <?> "primary expression"
  where
    structKV = do
      field <- expression <?> "field"
      -- TODO: Implement named field punning?
      value <- op TColon *> (expression <?> "value")
      pure (field, value)

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
      where
        args = expression `sepBy` hidden (op TComma) <?> "arguments"
    goGet c = EIndex c <$> (op TLBrack *> field) <*> op TRBrack
      where
        field = ident <&> ELit . LStr . (.lexeme) <?> "field"
    goIndex c = EIndex c <$> (op TLBrack *> (expression <?> "index")) <*> op TRBrack

unary, pow, factor, term, comparison, equality, logicAnd, logicOr :: Parser Expr
unary =
  ( EUnary
      <$> choice (op <$> [TBang, TPlus, TMinus])
      <*> (unary <?> "operand")
  )
    <|> call
pow = do
  lhs <- unary <?> "operand"
  lhs `option` (EBinary lhs <$> op TStar2 <*> pow)
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
    synced = withRecovery $ (*> (sync end $> EError)) . registerParseError
    end = choice $ op TSemi : (lookAhead . kw <$> [TTrue, TFalse, TNull, TStruct, TIf, TFun])

-- Declarations:

letDecl, funDecl :: Parser Decl
letDecl =
  DLet
    <$> ident
    <*> (op TEq *> (expression <?> "value"))
    & between (kw TLet) (op TSemi)
    <?> "variable declaration"
funDecl =
  DFun
    <$> (kw TFun *> (ident <?> "function name"))
    <*> paramList
    <*> (block <?> "function body")
    <?> "function declaration"

declaration :: Parser Decl
declaration = synced (choice [letDecl, funDecl] <?> "declaration")
  where
    synced = withRecovery $ (*> (sync end $> DError)) . registerParseError
    end = choice $ op TSemi : (lookAhead . kw <$> [TLet, TFun])

-- | Syncs the parser towards the given Token pattern parser.
--
-- For example, in a C statement,
-- this pattern should match at the end of the current statement,
-- or right before the next one.
--
-- See: <https://craftinginterpreters.com/parsing-expressions.html#synchronizing-a-recursive-descent-parser>
sync :: Parser Token -> Parser ()
sync = void . (anySingle `manyTill`)

program :: Parser Prog
program = uncurry Prog <$> declaration `manyTill_` expression <* eof <?> "program"
