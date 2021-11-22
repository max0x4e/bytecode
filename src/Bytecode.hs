{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields, LambdaCase #-}

module Bytecode
    ( 
      runInterpreter
    ) where

import Control.Monad.State.Lazy
    ( MonadTrans(lift), MonadState(get, put), State, StateT, runStateT, runState, gets, modify )
import Control.Monad.Cont ( ContT(..), liftIO )
import Data.Stack ( Stack, stackNew, stackIsEmpty, stackPush, stackPop ) 
import Data.HashMap.Strict as M
import Text.Regex
import Text.Read ( readMaybe )
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
data Value = Undef | Integer Integer | Double Double | Word String deriving (Show)

type Eval a = ContT Value (StateT IST IO) a

type DataStack = Stack Value

type CompilerStack = Stack ( String, Eval Value )

type Heap = [ Value ]

type Dictionary a = HashMap String a

data IST = IST { 
    ds :: DataStack,  -- runtime data stack
    cs :: CompilerStack, -- compile time stack for complex control structures
    heap :: Heap,     -- heap
    program :: [ String ], -- uncompiled part of the program
    pcode :: [ Eval Value ],   -- byte code
    dd :: Dictionary [ Eval Value ] -- dynamic dictionary
}

compilerActions :: Dictionary (Eval Value)
compilerActions = fromList [(":", cColon), (";", cSemi), ("if", cIf), ("else", cElse), ("then", cThen), ("begin", cBegin), ("until", cUntil)]

runtimeActions :: Dictionary (Eval Value)
runtimeActions = fromList [("+", rAdd), ("-" , rSub), ("/", rDiv), ("*", rMul), ("over", rOver), ("dup", rDup), ("swap", rSwap), (".", rDot), 
            ("dump" , rDump), ("drop", rDrop), ("=", rEq), (">", rGt), ("<", rLt), (",", rComa), ("@", rAt), ("!" , rBang), 
            ("allot", rAllot), ("create", rCreate), ("does>", rDoes)]

rData :: String -> Eval Value
rData w = case readMaybe w :: Maybe Integer of
    Just i -> return (Integer i)
    Nothing -> case readMaybe w :: Maybe Double of 
        Just d -> return (Double d)
        Nothing -> return (Word w)

rAdd :: Eval Value
rAdd = get >>= put >> return Undef

rSub :: Eval Value
rSub = get >>= put >> return Undef 

rDiv :: Eval Value
rDiv = get >>= put >> return Undef

rMul :: Eval Value
rMul = get >>= put >> return Undef

rOver :: Eval Value
rOver = get >>= put >> return Undef 

rDup :: Eval Value
rDup = get >>= put >> return Undef

rSwap :: Eval Value
rSwap = get >>= put >> return Undef

rDot :: Eval Value
rDot = get >>= put >> return Undef 

rDump :: Eval Value
rDump = get >>= put >> return Undef

rDrop :: Eval Value
rDrop = get >>= put >> return Undef

rEq :: Eval Value
rEq = get >>= put >> return Undef

rGt :: Eval Value
rGt = get >>= put >> return Undef 

rLt :: Eval Value
rLt = get >>= put >> return Undef 

rComa :: Eval Value
rComa = get >>= put >> return Undef 

rAt :: Eval Value
rAt = get >>= put >> return Undef 

rBang :: Eval Value
rBang = get >>= put >> return Undef 

rAllot :: Eval Value
rAllot = get >>= put >> return Undef

rCreate :: Eval Value
rCreate = get >>= put >> return Undef

rDoes :: Eval Value
rDoes = get >>= put >> return Undef

rRun :: Eval Value
rRun = get >>= put >> return Undef

rPush :: Eval Value
rPush = get >>= put >> return Undef

cColon :: Eval Value
cColon = get >>= put >> return Undef

cSemi :: Eval Value
cSemi = get >>= put >> return Undef

cIf :: Eval Value
cIf = get >>= put >> return Undef

cElse :: Eval Value
cElse = get >>= put >> return Undef

cThen :: Eval Value
cThen = get >>= put >> return Undef

cBegin :: Eval Value
cBegin = get >>= put >> return Undef

cUntil :: Eval Value
cUntil = get >>= put >> return Undef

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
                                                else case M.lookup x compilerActions of
                                                         Just op -> op >> compile
                                                         Nothing -> case M.lookup x runtimeActions of 
                                                                        Just op -> do
                                                                            modify $ \ist -> ist { pcode = ist.pcode ++ [op] }
                                                                            compile
                                                                        Nothing -> do
                                                                            dd <- gets dd
                                                                            case M.lookup x dd of
                                                                                Just bc -> do
                                                                                    modify $ \ist -> ist { pcode = ist.pcode ++ [rRun, rData x] }
                                                                                    compile
                                                                                Nothing -> do
                                                                                    y <- rData x
                                                                                    case y of
                                                                                        Integer i -> modify $ \ist -> ist { pcode = ist.pcode ++ [rPush, rData x] }
                                                                                        Double d -> modify $ \ist -> ist { pcode = ist.pcode ++ [rPush, rData x] }
                                                                                        Word w -> modify $ \ist -> ist { pcode = ist.pcode ++ [rRun, rData x] }
                                                                                    compile
        IST { pcode = (x:xs) } -> do  cs <- gets cs
                                      if stackIsEmpty cs then exec
                                      else do
                                          c <- liftIO $ moreCode "... " 
                                          modify $ \ist -> ist { program = c }
                                          compile

 
exec :: Eval Value
exec = get >>= \case
        IST { pcode = [] } -> compile
        IST { pcode = (x:xs) } -> x >> exec

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
