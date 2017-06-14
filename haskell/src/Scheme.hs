module Scheme (Expr(..)
              ) where


data Expr = Atom String
             | List [Expr]
             | Pair Expr Expr
             | Number Integer
             | String String
             | Bool Bool
             deriving (Show)


