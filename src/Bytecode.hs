{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields, LambdaCase #-}

module Bytecode
    ( 
      runInterpreter
    ) where

import Control.Monad.State.Lazy
    ( MonadTrans(lift), MonadState(get, put), State, StateT, runStateT, runState, gets, modify )
import Control.Monad.Cont ( ContT(..), liftIO )
import Data.Stack ( Stack, stackNew ) 
import Data.HashMap.Strict 
import Text.Regex
import System.IO 
import System.Exit
------------------------------------------------------------------
type MyState = String 
data Command = Return Int | GetState | SetState MyState | FuncCall [Command] deriving (Show)
data ValueOld = Undefined | ValueOld Int | StateValueOld MyState deriving (Show)

type EvalOld a = ContT ValueOld (State MyState) a

runEvalOld :: EvalOld ValueOld -> MyState -> (ValueOld, MyState)
runEvalOld evalOld = runState (runContT evalOld return) 

evalPCode :: [Command] -> ValueOld
evalPCode stmts = fst $ runEvalOld (evalBlock stmts) ""

-- compile
evalBlock :: [Command] -> EvalOld ValueOld
evalBlock [] = return Undefined
evalBlock [stmt] = evalStatment stmt
evalBlock (st:stmts) = evalStatment st >> evalBlock stmts

-- eval
evalStatment :: Command -> EvalOld ValueOld
evalStatment (Return val) = ContT $ \_ -> return (ValueOld val)
evalStatment (SetState state) = put state >> return Undefined
evalStatment (FuncCall stmts) = lift $ runContT (evalBlock stmts) return
evalStatment GetState = StateValueOld <$> get

------------------------------------------------------------------
data Value = Undef | Integer Integer | Double Double deriving (Show)

type Eval a = ContT Value (StateT IST IO) a

type DataStack = Stack Value

type CompilerStack = Stack ( String, ByteCodeOp )

type Heap = [ Value ]

type ByteCodeOp = IST -> IST

type ByteCode = [ ByteCodeOp ]

type Dictionary a = HashMap String a

data IST = IST { 
    ds :: DataStack,  -- runtime data stack
    cs :: CompilerStack, -- compile time stack for complex control structures
    heap :: Heap,     -- heap
    program :: [ String ], -- uncompiled part of the program
    pcode :: ByteCode,   -- byte code
    dd :: Dictionary ByteCode -- dynamic dictionary
}

compilerActions :: Dictionary ByteCodeOp
compilerActions = fromList [(":", cColon), (";", cSemi), ("if", cIf), ("else", cElse), ("then", cThen), ("begin", cBegin), ("until", cUntil)]

runtimeActions :: Dictionary ByteCodeOp
runtimeActions = fromList [("+", rAdd), ("-" , rSub), ("/", rDiv), ("*", rMul), ("over", rOver), ("dup", rDup), ("swap", rSwap), (".", rDot), 
            ("dump" , rDump), ("drop", rDrop), ("=", rEq), (">", rGt), ("<", rLt), (",", rComa), ("@", rAt), ("!" , rBang), 
            ("allot", rAllot), ("create", rCreate), ("does>", rDoes)]

rAdd :: ByteCodeOp
rAdd s = s

rSub :: ByteCodeOp
rSub s = s 

rDiv :: ByteCodeOp
rDiv s = s

rMul :: ByteCodeOp
rMul s = s

rOver :: ByteCodeOp
rOver s = s 

rDup :: ByteCodeOp
rDup s = s

rSwap :: ByteCodeOp
rSwap s = s

rDot :: ByteCodeOp
rDot s = s 

rDump :: ByteCodeOp
rDump s = s

rDrop :: ByteCodeOp
rDrop s = s

rEq :: ByteCodeOp
rEq s = s

rGt :: ByteCodeOp
rGt s = s 

rLt :: ByteCodeOp
rLt s = s 

rComa :: ByteCodeOp
rComa s = s 

rAt :: ByteCodeOp
rAt s = s 

rBang :: ByteCodeOp
rBang s = s 

rAllot :: ByteCodeOp
rAllot s = s

rCreate :: ByteCodeOp
rCreate s = s

rDoes :: ByteCodeOp
rDoes s = s

cColon :: ByteCodeOp
cColon s = s

cSemi :: ByteCodeOp
cSemi s = s

cIf :: ByteCodeOp
cIf s = s

cElse :: ByteCodeOp
cElse s = s

cThen :: ByteCodeOp
cThen s = s

cBegin :: ByteCodeOp
cBegin s = s

cUntil :: ByteCodeOp
cUntil s = s

runEval :: Eval Value -> IST -> IO (Value, IST)
runEval eval = runStateT (runContT eval return)

evalProg :: String -> IO (Value, IST)
evalProg program = 
    let st = IST {
        ds = stackNew,
        cs = stackNew,
        heap = [],
        program = lexer $ program,
        pcode = [],
        dd = empty
    } 
    in
        runEval compile st

moreCode :: String -> IO [ String ]
moreCode prompt = do
    putStr prompt
    hFlush stdout
    rawCode <- getLine
    return $ lexer $ rawCode

compile :: Eval Value
compile = get >>= \case
        IST { program = [], pcode = [] } -> do
            c <- liftIO $ moreCode "bc> " 
            modify $ \ist -> ist { program = c }
            compile
        IST { program = (x:xs), pcode = [] } -> if x == ":bye" 
                                                then do
                                                    liftIO $ putStrLn "Good bye!"
                                                    liftIO $ exitSuccess 
                                                else if member x compilerActions then do
                                                    liftIO $ putStrLn "cAct"
                                                    return Undef
                                                else if member x runtimeActions then do 
                                                    liftIO $ putStrLn "rAct"
                                                    return Undef
                                                else do
                                                    dd <- gets dd
                                                    if member x dd then do
                                                        liftIO $ putStrLn "dynamic"
                                                        return Undef
                                                    else do
                                                        liftIO $ print $ x
                                                        liftIO $ print $ xs
                                                        return Undef
        IST { pcode = (x:xs) } -> exec
 
exec :: Eval Value
exec = get >>= \case
        IST { pcode = [] } -> compile
        IST { pcode = (x:xs) } -> exec

-- Later I may make it fancier with attoparsec or smth similar
lexer :: String -> [ String ]
lexer input = words $ subRegex (mkRegex "#.*$") input "\n"

printout :: IST -> IO()
printout IST{ program = [] } = print $ "one"
printout IST{ program = (x:p) } = print $ x

runInterpreter :: String -> IO ()
runInterpreter initCode = do
    (val, state) <- evalProg initCode
    print $ val
    -- print $ evalPCode [SetState "Hello", FuncCall [SetState initCode, Return 3], GetState] 
