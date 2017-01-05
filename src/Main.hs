{-# Language MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module Main where

import Prelude hiding (lookup, print)
import qualified Data.Map as Map
import Data.Maybe
import qualified System.IO as System
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

data Val = I Int | B Bool
    deriving (Eq, Show, Read)

data Expr = Const Val
    | Add Expr Expr | Sub Expr Expr  | Mul Expr Expr | Div Expr Expr
    | And Expr Expr | Or Expr Expr | Not Expr
    | Eq Expr Expr | Gt Expr Expr | Lt Expr Expr
    | Var String
    deriving (Eq, Show, Read)

data Statement = Assign String Expr
    | If Expr Statement Statement
    | While Expr Statement
    | Print Expr
    | Seq Statement Statement
    | Try Statement Statement
    | Pass
    | Step Statement
    deriving (Eq, Show, Read)

type Name = String
type Env = Map.Map Name Val

type Eval a = ReaderT Env (ExceptT String Identity) a

lookup k t = case Map.lookup k t of
    Just x -> return x
    Nothing -> fail ("Unknown variable "++k)

runEval env expr = runIdentity $ runExceptT $ runReaderT expr env

evali op e0 e1 = do
    e0' <- eval e0
    e1' <- eval e1
    case (e0', e1') of
        (I i0, I i1) -> return $ I (i0 `op` i1)
        _            -> fail "type error in arithmetic expression"

evalb op e0 e1 = do
    e0' <- eval e0
    e1' <- eval e1
    case (e0', e1') of
        (B i0, B i1) -> return $ B (i0 `op` i1)
        _            -> fail "type error in boolean expression"

evalib op e0 e1 = do
    e0' <- eval e0
    e1' <- eval e1
    case (e0', e1') of
        (I i0, I i1) -> return $ B (i0 `op` i1)
        _            -> fail "type error in arithmetic expression"

eval :: Expr -> Eval Val
eval (Const v) = return v
eval (Add e0 e1) = do evali (+) e0 e1
eval (Sub e0 e1) = do evali (-) e0 e1
eval (Mul e0 e1) = do evali (*) e0 e1
eval (Div e0 e1) = do evali div e0 e1
eval (And e0 e1) = do evalb (&&) e0 e1
eval (Or e0 e1) = do evalb (||) e0 e1

eval (Not e0  ) = do
    evalb (const not) e0 (Const (B True))
    where not2 a _ = not a

eval (Eq e0 e1) = do evalib (==) e0 e1
eval (Gt e0 e1) = do evalib (>) e0 e1
eval (Lt e0 e1) = do evalib (<) e0 e1

eval (Var s) = do env <- ask
                  lookup s env

type Run a = StateT Env (ExceptT String IO) a
runRun p =  runExceptT ( runStateT p Map.empty)

set :: (Name, Val) -> Run ()
set (s,i) = state $ (\table -> ((), Map.insert s i table))

exec :: Statement -> Run ()

exec (Assign s v) = do
    st <- get
    Right val <- return $ runEval st (eval v)
    set (s,val)

exec (Seq s0 s1) = do exec (Step s0) >> exec (Step s1)

exec (Print e) = do
    st <- get
    Right val <- return $ runEval st (eval e)
    liftIO $ System.print val
    return ()

exec (If cond s0 s1) = do
    st <- get
    Right (B val) <- return $ runEval st (eval cond)
    if val then do exec s0 else do exec s1

exec (While cond s) = do
    st <- get
    Right (B val) <- return $ runEval st (eval cond)
    if val then do exec s >> exec (While cond s) else return ()

exec (Try s0 s1) = do catchError (exec s0) (\e -> exec s1)
exec Pass = return ()

exec (Step (Seq s0 s1)) = exec (Seq s0 s1)

exec (Step s) = do
    askAction s

askAction :: Statement -> Run ()
askAction s = do
    liftIO $ putStrLn (show s ++ "\nEnter anything to execute this statement")
    action <- liftIO getLine
    case action of
        _ -> exec s

type Program = Writer Statement ()

int = Const . I
bool = Const . B
var = Var

class SmartAssignment a where
    assign :: String -> a -> Statement

instance SmartAssignment Int where
    assign v i = Assign v (Const (I i))

instance SmartAssignment Bool where
    assign v b = Assign v (Const (B b))

instance SmartAssignment Expr where
    assign v e = Assign v e

class PrettyExpr a b where
    (.*) :: a -> b -> Expr
    (.-) :: a -> b -> Expr

instance PrettyExpr String String where
    x .* y = (Var x) `Mul` (Var y)
    x .- y = (Var x) `Sub` (Var y)

instance PrettyExpr String Int where
    x .* y = (Var x) `Mul` (Const (I y))
    x .- y = (Var x) `Sub` (Const (I y))

instance Monoid Statement where
    mempty = Pass
    mappend a b = a `Seq` b

compile :: Program -> Statement
compile p = snd . runIdentity $ runWriterT p

run :: Program -> IO ()
run program = do
    result <- runExceptT $ runStateT (exec (compile program)) Map.empty
    case result of
        Right ( (), env ) -> return ()
        Left exn -> System.print ("Uncaught exception: "++exn)

infixl 1 .=
(.=) :: String -> Expr -> Program
var .= val = tell $ assign var val

iif :: Expr -> Program -> Program -> Program
iif cond tthen eelse = tell $ If cond (compile tthen) (compile eelse)

while :: Expr -> Program -> Program
while cond body = tell $ While cond (compile body)

print :: Expr -> Program
print e = tell $ Print e

try :: Program -> Program -> Program
try block recover = tell $ Try (compile block) (compile recover)

prog10 :: Program
prog10 = do
    "arg"     .= int 10
    "scratch" .= var "arg"
    "total"   .= int 1
    while ( (var "scratch") `Gt` (int 1) ) (
     do "total"   .=  "total" .* "scratch"
        "scratch" .= "scratch" .- (1::Int)
        print $ var "scratch"
     )
    print $ var "total"

stringToProgram :: String -> Program
stringToProgram src = (read :: String -> Program) src

main :: IO ()
main = do
    file <- readFile "factorial.txt"
    run (stringToProgram file)