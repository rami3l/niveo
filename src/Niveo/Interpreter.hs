module Niveo.Interpreter where

import Niveo.Parser (Expr)
import Relude

type Env = HashMap Text Expr

eval :: Expr -> State Env (Either Text Expr)
eval = undefined
