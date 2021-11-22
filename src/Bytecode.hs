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
import Control.Applicative hiding ( empty )

data Value = Undef | Integer Int | Float Float | Word String deriving (Show)

type Eval a = ContT Value (StateT IST IO) a

type DataStack = Stack Value

type CompilerStack = Stack ( String, Eval Value )

type Heap = [ Value ]

type ByteCodeOp = Eval Value

type Dictionary a = HashMap String a

data IST = IST {
    pp :: Int, -- bytecode op pointer
    hp :: Int, -- heap pointer
    ds :: DataStack,  -- runtime data stack
    cs :: CompilerStack, -- compile time stack for complex control structures
    heap :: Heap,     -- heap
    program :: [ String ], -- uncompiled part of the program
    pcode :: [ ByteCodeOp ],   -- byte code
    dd :: Dictionary [ ByteCodeOp ] -- dynamic dictionary
}

compilerActions :: Dictionary ByteCodeOp
compilerActions = fromList [(":", cColon), (";", cSemi), ("if", cIf), ("else", cElse), ("then", cThen), ("begin", cBegin), ("until", cUntil)]

runtimeActions :: Dictionary ByteCodeOp
runtimeActions = fromList [("+", rAdd), ("-" , rSub), ("/", rDiv), ("*", rMul), ("over", rOver), ("dup", rDup), ("swap", rSwap), (".", rDot), 
            ("dump" , rDump), ("drop", rDrop), ("=", rEq), (">", rGt), ("<", rLt), (",", rComa), ("@", rAt), ("!" , rBang), 
            ("allot", rAllot), ("create", rCreate), ("does>", rDoes)]

popData :: Eval Value
popData = do
    ds <- gets ds
    case stackPop ds of
        Just (ds, x) -> return x
        Nothing -> liftIO $ die "Attempted to pop on empty data stack"

pushData :: Value -> Eval Value
pushData x = do 
    ist <- get
    put ist { ds = stackPush ist.ds x }
    return Undef

rData :: String -> ByteCodeOp
rData w = case readMaybe w :: Maybe Int of
    Just i -> return (Integer i)
    Nothing -> case readMaybe w :: Maybe Float of 
        Just f -> return (Float f)
        Nothing -> return (Word w)

rAdd :: ByteCodeOp
rAdd = do
    b <- popData
    a <- popData
    case (a,b) of 
        (Integer a', Integer b') -> pushData (Integer (a' + b'))
        (Float a', Float b') -> pushData (Float (a' + b'))
    return Undef

rSub :: ByteCodeOp
rSub = do
    b <- popData
    a <- popData
    case (a,b) of 
        (Integer a', Integer b') -> pushData (Integer (a' - b'))
        (Float a', Float b') -> pushData (Float (a' - b'))
    return Undef

rDiv :: ByteCodeOp
rDiv = do
    b <- popData
    a <- popData
    case (a,b) of 
        (Integer a', Integer b') -> pushData (Integer (a' `div` b'))
        (Float a', Float b') -> pushData (Float (a' / b'))
    return Undef

rMul :: ByteCodeOp
rMul = do
    b <- popData
    a <- popData
    case (a,b) of 
        (Integer a', Integer b') -> pushData (Integer (a' * b'))
        (Float a', Float b') -> pushData (Float (a' * b'))
    return Undef

rOver :: ByteCodeOp
rOver = do
    a <- popData
    b <- popData
    pushData b
    pushData a
    pushData b
    return Undef 

rDup :: ByteCodeOp
rDup = do
    a <- popData
    pushData a
    pushData a
    return Undef

rSwap :: ByteCodeOp
rSwap = do
    a <- popData
    b <- popData
    pushData a
    pushData b
    return Undef

rDot :: ByteCodeOp
rDot = do
    a <- popData
    liftIO $ print a
    return Undef

rDump :: ByteCodeOp
rDump = do
    ds <- gets ds
    liftIO $ print ds
    return Undef

rDrop :: ByteCodeOp
rDrop = popData >> return Undef

rEq :: ByteCodeOp
rEq = do
    b <- popData
    a <- popData
    case (a,b) of 
        (Integer a', Integer b') -> pushData (Integer (if a' == b' then 1 else 0))
        (Float a', Float b') -> pushData (Float (if a' == b' then 1 else 0))
    return Undef

rGt :: ByteCodeOp
rGt = do
    b <- popData
    a <- popData
    case (a,b) of 
        (Integer a', Integer b') -> pushData (Integer (if a' > b' then 1 else 0))
        (Float a', Float b') -> pushData (Float (if a' > b' then 1 else 0))
    return Undef

rLt :: ByteCodeOp
rLt = do
    b <- popData
    a <- popData
    case (a,b) of 
        (Integer a', Integer b') -> pushData (Integer (if a' < b' then 1 else 0))
        (Float a', Float b') -> pushData (Float (if a' < b' then 1 else 0))
    return Undef

rComa :: ByteCodeOp
rComa = get >>= put >> return Undef 

rAt :: ByteCodeOp
rAt = do
    ist <- get
    addr <- popData
    case addr of
        Integer a -> pushData (ist.heap !! a)
        _ -> liftIO $ die "Heap address is incorrect."

rBang :: ByteCodeOp
rBang = get >>= put >> return Undef 

rAllot :: ByteCodeOp
rAllot = get >>= put >> return Undef

rCreate :: ByteCodeOp
rCreate = get >>= put >> return Undef

rDoes :: ByteCodeOp
rDoes = get >>= put >> return Undef

rRun :: ByteCodeOp
rRun = get >>= put >> return Undef

rPush :: ByteCodeOp
rPush = do
    ist <- get
    d <- ist.pcode !! ist.pp
    pushData (d)
    put ist { pp = ist.pp + 1 }
    return Undef

cColon :: ByteCodeOp
cColon = get >>= put >> return Undef

cSemi :: ByteCodeOp
cSemi = get >>= put >> return Undef

cIf :: ByteCodeOp
cIf = get >>= put >> return Undef

cElse :: ByteCodeOp
cElse = get >>= put >> return Undef

cThen :: ByteCodeOp
cThen = get >>= put >> return Undef

cBegin :: ByteCodeOp
cBegin = get >>= put >> return Undef

cUntil :: ByteCodeOp
cUntil = get >>= put >> return Undef

runEval :: Eval Value -> IST -> IO (Value, IST)
runEval eval = runStateT (runContT eval return)

evalProg :: String -> IO (Value, IST)
evalProg program = 
    let st = IST {
        pp = 0,
        hp = 0,
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
                                                                                        Float d -> modify $ \ist -> ist { pcode = ist.pcode ++ [rPush, rData x] }
                                                                                        Word w -> modify $ \ist -> ist { pcode = ist.pcode ++ [rRun, rData x] }
                                                                                    compile
        IST { pcode = (x:xs) } -> do  cs <- gets cs
                                      if stackIsEmpty cs then exec
                                      else do
                                          c <- liftIO $ moreCode "... " 
                                          modify $ \ist -> ist { program = c }
                                          compile

exec :: Eval Value
exec = do
        ist <- get
        if ist.pp == length ist.pcode then compile
        else put ist { pp = ist.pp + 1 } >> ist.pcode !! ist.pp >> exec

-- Later I may make it fancier with attoparsec or smth similar
lexer :: String -> [ String ]
lexer input = words $ subRegex (mkRegex "#.*$") input "\n"

runInterpreter :: String -> IO ()
runInterpreter initCode = do
    (val, state) <- evalProg initCode
    print $ val 
