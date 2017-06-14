module Eval (
              evaluate
            ) where

import Scheme
import Control.Monad

type Var = String
newtype Lambda = Lam (Var, Expr) deriving (Show)

type Env = (Var -> Either String DomainValue)

data DomainValue =
  Closure (Lambda, Env)
  | StringLiteral String
  | NumLiteral Integer
  | Boolean Bool
  | VList [DomainValue]
  | VPair DomainValue DomainValue

instance Show DomainValue where
  show (Closure _) = "closure"
  show (StringLiteral s) = s
  show (NumLiteral i) = show i
  show (Boolean b) = show b
  show (VList xs) = foldMap show xs
  show (VPair a b) = "(" ++ (show a) ++ "," ++ (show b) ++ ")"
 
data Cont = Empty | Ar (Expr, Env, Cont) | Fn (Lambda, Env, Cont)

-- CEK Machine State
-- C := Expression, E := Env, K := Cont
type EvalState = (Expr, Env, Cont)

instance Show Cont where
  show Empty = "empty"
  show (Ar (_,_,_)) = "argument"
  show (Fn (_,_,_)) = "function call"
  
getExpr :: EvalState -> Expr
getExpr (e, _, _) = e

getEnv :: EvalState -> Env
getEnv (_, e, _) = e

getCont :: EvalState -> Cont
getCont (_,_,c) = c

inject :: Expr -> EvalState
inject e  = (e, rho, Empty)
    where rho = const $ Left ("not bound value: " ++ (show e))

step :: EvalState -> Either String EvalState

step (List (Atom "lambda" : List [Atom param] : body),
      env,
      Ar(e, env', cont)) =
  let lam = Lam (param, List body) in
    Right (e, env', Fn (lam, env, cont))

step (List (Atom "lambda" : List [Atom param] : body),
      env,
      Fn(Lam (arg, b), env', cont)) =
   Right (b, extend env', cont)
  where
    lam = Lam (param, List body)
    extend e x = if x == arg then Right (Closure (lam, env)) else e x

step (List [], env, Ar(e, env', cont)) =  Right (e, env', cont)
step (List [], env, Fn(Lam(param, List body), env', cont)) =  
   Right (List (Atom "lambda" : List [Atom param] : body), env', cont)

step (List (x : xs), env, Fn(lam, env', cont)) =
  Right (x, env, Fn(lam, env', Ar(List xs, env, cont)))
  
step (List (x : xs), env, cont) =
  Right (x, env, Ar(List xs, env, cont))


step (Atom atom, env, cont) = case env atom of
   Right(Closure (Lam(v, body), env')) ->
     Right (List (Atom "lambda" : List [Atom v] : [body]), env', cont)
   Right(StringLiteral s) -> Right (String s, env, cont)
   Right(NumLiteral i)    -> Right (Number i, env, cont)
   Left e                -> Left e
   

step (String i, env, Fn(Lam(arg, b), env', cont)) = Right (b, extend env', cont)
   where extend e x = if x == arg then Right (StringLiteral i) else e x

step (Number i, env, Fn(Lam(arg, b), env', cont)) = Right (b, extend env', cont)
   where extend e x = if x == arg then Right (NumLiteral i) else e x

step (Bool i, env, Fn(Lam(arg, b), env', cont)) = Right  (b, extend env', cont)
   where extend e x = if x == arg then Right (Boolean i) else e x

step (e, env, Ar(e', env', cont)) = Right (e', env', Ar(e, env, cont))
--step (e, env, Empty) = Right (e, env, Empty)
step _ = Left "stuck"

isFinal :: EvalState -> Bool
isFinal (_, _, Empty) = True
isFinal _ = False

terminal :: Integer -> [(Expr, Cont)] -> Either String EvalState -> Either String (EvalState, Integer, [(Expr, Cont)])
terminal n l (Right state) 
  | isFinal state = Right (state, n, (getExpr state, getCont state) : l )
  | otherwise = terminal (n + 1) ((getExpr state, getCont state) : l) (step state) 
terminal _ l (Left s) = Left $ s ++ (show l)

eval :: Expr -> Either String (EvalState, Integer, [(Expr, Cont)])
eval e = terminal 0 [] $ step $ inject e

evaluate :: Expr -> Either String DomainValue
evaluate e = case eval e of
  Right (s, _, _) -> (interpret (getEnv s) $ getExpr s)
  Left err -> Left err
  where
    interpret _ (Bool b) = Right $ Boolean b
    interpret _ (Number i) = Right $ NumLiteral i
    interpret _ (String s) = Right $ StringLiteral s
    interpret env (Atom x) = env x
    interpret env (List xs) = VList <$> traverse (interpret env) xs
    interpret env (Pair a b) = VPair <$> interpret env a <*> interpret env b

test :: Expr -> IO ()
test e = case eval e of
  Right (s, n, l) -> do
    _ <- putStrLn $ "Number of steps: " ++ (show n)
    putStrLn $ "list of steps: " ++ (show (reverse l)) 
  Left err -> putStrLn err
