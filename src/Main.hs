{-# Language MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module Main where

import Prelude hiding (lookup, print)
import qualified Data.Map as Map
import Data.Maybe
import Data.List
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
data MetaState = MetaState {environments :: [Env], statements :: [Statement]}

-- Color magic strings
colorRed = "\x1b[31m"
colorGreen = "\x1b[32m"
colorYellow = "\x1b[33m"
colorBlue = "\x1b[34m"
colorMagenta = "\x1b[35m"
colorCyan = "\x1b[36m"
colorReset = "\x1b[0m"

-- Returns latest environment (variable dictionary)
getEnv :: MetaState -> Env
getEnv ms = (if l == 0 then Map.empty else (head $ environments ms))
    where l = length (environments ms)

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
                  Main.lookup s env

type Run a = StateT MetaState (ExceptT String IO) a
runRun p =  runExceptT ( runStateT p (MetaState {environments = [], statements = []}))

set :: (Name, Val) -> Run ()
set (s,i) = state $ (\table ->
    let newEnv = Map.insert s i $ getEnv table
    in ((), MetaState {environments = newEnv : (tail $ environments table), statements = (statements table)} ))

exec :: Statement -> Run ()

exec (Assign s v) = do
    st <- get
    Right val <- return $ runEval (getEnv st) (eval v)
    set (s,val)

exec (Seq s0 s1) = do exec (Step s0) >> exec (Step s1)

exec (Print e) = do
    st <- get
    Right val <- return $ runEval (getEnv st) (eval e)
    liftIO $ System.print val
    return ()

exec (If cond s0 s1) = do
    st <- get
    Right (B val) <- return $ runEval (getEnv st) (eval cond)
    if val then do exec s0 else do exec s1

exec (While cond s) = do
    st <- get
    Right (B val) <- return $ runEval (getEnv st) (eval cond)
    if val then do exec s >> exec (While cond s) else return ()

exec (Try s0 s1) = do catchError (exec s0) (\e -> exec s1)
exec Pass = return ()

exec (Step (Seq s0 s1)) = exec (Seq s0 s1)

exec (Step s) = do
    askAction s

-- Properly print a statement
showSt :: Statement -> String
showSt (If c _ _)  = "If " ++ (show c) ++ " {...}"
showSt (While c _) = "While " ++ (show c) ++ " {...}"
showSt s = show s

putStrC :: String -> String -> IO ()
putStrC c s = putStr $ c ++ s ++ colorReset

-- Displays the latest environments (and therefore variables history)
showVariables :: Run ()
showVariables = do
    st <- get
    case (length $ environments st) of
        0 -> liftIO $ putStrC colorRed "No variables have been set.\n"
        _ -> do
            forM_ (zip [0..] (reverse $ environments st)) $ \(i, e) -> do
                liftIO $ putStrC colorGreen  ("--- State " ++ (show i) ++ " ---\n")
                liftIO $ putStrC colorGreen ((Map.foldrWithKey (\k v r -> r ++ (show k) ++ ": " ++ (show v) ++ "\n") "" e) ++ "\n")

-- Records the current environment and the statement about to be executed
recordState :: Statement -> Run ()
recordState s = do
    st <- get
    put $ MetaState {environments = (getEnv st):(environments st), statements = s:(statements st)}

-- Takes a step back in the execution
rewindAction :: Statement -> Run ()
rewindAction s = do
    st <- get
    case (length $ statements st) of
        0 -> do
            liftIO $ putStrC colorRed "Error: no previous statements.\n"
            exec (Step s)
        _ -> do
            let previous = head $ statements st
            put $ MetaState {environments = (tail $ environments st), statements = (tail $ statements st)}
            -- We need the execute (ask for an action on) the previous statement, and then we will go back to the current statement
            exec (Step previous)
            exec (Step s)

-- Asks the user for an action regarding the current statement
askAction :: Statement -> Run ()
askAction s = do
    liftIO $ putStrC colorBlue ("> " ++ showSt s ++ "\n")
    liftIO $ putStr "What do you want to do? [exec e | inspect i | rewind r]\n"
    action <- liftIO getLine
    case action of
        "e" -> do
            recordState s
            exec s
        "i" -> do
            showVariables
            askAction s
        "r" -> do
            rewindAction s
        _ -> do
            liftIO $ putStrC colorRed ("Unknown command.\n")
            askAction s

type Program = Writer Statement ()

compile :: Program -> Statement
compile p = snd . runIdentity $ runWriterT p

run :: Program -> IO ()
run program = do
    result <- runExceptT $ runStateT (exec (compile program)) (MetaState {environments = [], statements = []})
    case result of
        Right ( (), env ) -> return ()
        Left exn -> System.print ("Uncaught exception: "++exn)

stringToProgram :: String -> Program
stringToProgram src = (read :: String -> Program) src

-- Detect unused variables using two lists
-- Used (in any Expr) and Init (in any Assign)

detect :: Program -> [String]
detect src = (nub $ analyzeInit (compile src)) \\ (nub $ analyzeUsed (compile src))

analyzeUsed :: Statement -> [String]
analyzeUsed (Step s) = analyzeUsed s
analyzeUsed (Seq s0 s1) = analyzeUsed s0 ++ analyzeUsed s1
analyzeUsed (Assign _ e) = used e
analyzeUsed (Print e) = used e
analyzeUsed (If c s0 s1) = used c ++ analyzeUsed s0 ++ analyzeUsed s1
analyzeUsed (While c s) = used c ++ analyzeUsed s

analyzeInit :: Statement -> [String]
analyzeInit (Step s) = analyzeInit s
analyzeInit (Seq s0 s1) = analyzeInit s0 ++ analyzeInit s1
analyzeInit (Assign n _) = [n]
analyzeInit (Print _) = []
analyzeInit (If _ s0 s1) = analyzeInit s0 ++ analyzeInit s1
analyzeInit (While _ s) = analyzeInit s

used :: Expr -> [String]
used (Add e0 e1) = used e0 ++ used e1
used (Sub e0 e1) = used e0 ++ used e1
used (Mul e0 e1) = used e0 ++ used e1
used (Div e0 e1) = used e0 ++ used e1
used (And e0 e1) = used e0 ++ used e1
used (Or  e0 e1) = used e0 ++ used e1
used (Eq  e0 e1) = used e0 ++ used e1
used (Gt  e0 e1) = used e0 ++ used e1
used (Lt  e0 e1) = used e0 ++ used e1
used (Var name) = [name]
used (Not e0) = used e0
used (Const _) = []

main :: IO ()
main = do
    file <- readFile "factorial.txt"
    putStrLn "Reading file factorial.txt..."
    let analysis = detect $ stringToProgram file
    if (length analysis == 0)
    then putStrLn $ colorGreen ++ "The program is correct.\n"
    else putStrLn $ colorRed ++ "Warning! Some variables have been initialized but not used: " ++ (show analysis)
    run (stringToProgram file)