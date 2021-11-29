{-# LANGUAGE LambdaCase, TemplateHaskell #-}

module Bytecode
    ( 
      runInterpreter
    ) where

import Control.Monad.State.Lazy
    ( MonadTrans(lift), MonadState(get, put), State, StateT, runStateT, runState, gets, modify )
import Control.Monad.Cont ( ContT(..), liftIO )
import Data.Stack ( Stack, stackNew, stackIsEmpty, stackPush, stackPop, stackPeek ) 
import Data.HashMap.Strict as M
import Text.Regex
import Text.Read ( readMaybe )
import System.IO 
import System.Exit
import Control.Lens 
import Control.Lens.TH (makeLenses)
data Value = Undef | Integer Int | Float Float | Word String deriving (Show)

type Eval a = ContT Value (StateT IST IO) a

type DataStack = Stack Value

type CompilerStack = Stack String

type Heap = [ Value ]

type ByteCodeOp = Eval Value

type Dictionary a = HashMap String a

data IST = IST {
    _pp :: Int, -- bytecode op pointer
    _hp :: Int, -- heap pointer
    _ds :: DataStack,  -- runtime data stack
    _cs :: CompilerStack, -- compile time stack for complex control structures
    _heap :: Heap,     -- heap
    _program :: [ String ], -- uncompiled part of the program
    _pcode :: [ ByteCodeOp ],   -- byte code
    _dd :: Dictionary [ ByteCodeOp ] -- dynamic dictionary
}

makeLenses ''IST

{-------------  Runtime Actions ----------------------------}

runtimeActions :: Dictionary ByteCodeOp
runtimeActions = fromList [("+", rAdd), ("-" , rSub), ("/", rDiv), ("*", rMul), ("over", rOver), ("dup", rDup), ("swap", rSwap), (".", rDot), 
            ("dump" , rDump), ("drop", rDrop), ("=", rEq), (">", rGt), ("<", rLt), (",", rComa), ("@", rAt), ("!" , rBang), 
            ("allot", rAllot), ("create", rCreate), ("does>", rDoes)]

popd :: Eval Value
popd = do
    s <- get
    case stackPop $ s^.ds of
        Just (xs, x) -> return x
        Nothing -> liftIO $ die "Attempted to pop on empty stack"

pushd :: Value -> Eval Value
pushd x = do 
    s <- get
    put s{ _ds = stackPush (s^.ds) x }
    return Undef

rData :: String -> ByteCodeOp
rData w = case readMaybe w :: Maybe Int of
    Just i -> return (Integer i)
    Nothing -> case readMaybe w :: Maybe Float of 
        Just f -> return (Float f)
        Nothing -> return (Word w)

rAdd :: ByteCodeOp
rAdd = do 
    b <- popd
    a <- popd
    case (a,b) of 
        (Integer a', Integer b') -> pushd (Integer (a' + b'))
        (Float a', Float b') -> pushd (Float (a' + b'))
    return Undef

rSub :: ByteCodeOp
rSub = do 
    b <- popd
    a <- popd
    case (a,b) of 
        (Integer a', Integer b') -> pushd (Integer (a' - b'))
        (Float a', Float b') -> pushd (Float (a' - b'))
    return Undef

rMul :: ByteCodeOp
rMul = do 
    b <- popd
    a <- popd
    case (a,b) of 
        (Integer a', Integer b') -> pushd (Integer (a' * b'))
        (Float a', Float b') -> pushd (Float (a' * b'))
    return Undef

rDiv :: ByteCodeOp
rDiv = do 
    b <- popd
    a <- popd
    case (a,b) of 
        (Integer a', Integer b') -> pushd (Integer (a' `div` b'))
        (Float a', Float b') -> pushd (Float (a' / b'))
    return Undef

rOver :: ByteCodeOp
rOver = do
    a <- popd
    b <- popd
    pushd b
    pushd a
    pushd b
    return Undef 

rDup :: ByteCodeOp
rDup = do
    a <- popd
    pushd a
    pushd a
    return Undef

rSwap :: ByteCodeOp
rSwap = do
    a <- popd
    b <- popd
    pushd a
    pushd b
    return Undef

rDot :: ByteCodeOp
rDot = do
    a <- popd
    liftIO $ print a
    return Undef

rDump :: ByteCodeOp
rDump = do
    s <- get
    liftIO $ print $ s ^. ds
    return Undef

rDrop :: ByteCodeOp
rDrop = popd >> return Undef

rJmp :: ByteCodeOp
rJmp = get >>= put >> return Undef

rJnz :: ByteCodeOp
rJnz = get >>= put >> return Undef

rJz :: ByteCodeOp
rJz = get >>= put >> return Undef

rEq :: ByteCodeOp
rEq = do
    b <- popd
    a <- popd
    case (a,b) of 
        (Integer a', Integer b') -> pushd (Integer (if a' == b' then 1 else 0))
        (Float a', Float b') -> pushd (Float (if a' == b' then 1 else 0))
    return Undef

rGt :: ByteCodeOp
rGt = do
    b <- popd
    a <- popd
    case (a,b) of 
        (Integer a', Integer b') -> pushd (Integer (if a' > b' then 1 else 0))
        (Float a', Float b') -> pushd (Float (if a' > b' then 1 else 0))
    return Undef

rLt :: ByteCodeOp
rLt = do
    b <- popd
    a <- popd
    case (a,b) of 
        (Integer a', Integer b') -> pushd (Integer (if a' < b' then 1 else 0))
        (Float a', Float b') -> pushd (Float (if a' < b' then 1 else 0))
    return Undef

rComa :: ByteCodeOp
rComa = get >>= put >> return Undef 

rAt :: ByteCodeOp
rAt = get >>= put >> return Undef

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
rPush = get >>= put >> return Undef

exec :: Eval Value
exec = get >>= put >> return Undef

{-------------  Compile-time Actions ----------------------------}
compilerActions :: Dictionary ByteCodeOp
compilerActions = fromList [(":", cColon), (";", cSemi), ("if", cIf), ("else", cElse), ("then", cThen), ("begin", cBegin), ("until", cUntil)]

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
    let st = IST 0 0 stackNew stackNew [] (lexer program) [] empty in
        runEval compile st

moreCode :: String -> IO [ String ]
moreCode prompt = do
    putStr prompt
    hFlush stdout
    lexer <$> getLine

compile :: Eval Value
compile = get >>= \case
        IST { _program = [], _pcode = [] } -> do
            c <- liftIO $ moreCode "bc> " 
            modify $ \s -> s { _program = c }
            compile
        IST { _program = (x:xs), _pcode = [] } -> if x == ":bye" 
                                                then do
                                                    liftIO $ putStrLn "Good bye!"
                                                    liftIO $ exitSuccess 
                                                else case M.lookup x compilerActions of
                                                         Just op -> op >> compile
                                                         Nothing -> case M.lookup x runtimeActions of 
                                                                        Just op -> do
                                                                            modify $ \s -> s { _pcode = s^.pcode ++ [op] }
                                                                            compile
                                                                        Nothing -> do
                                                                            dd <- gets _dd
                                                                            case M.lookup x dd of
                                                                                Just bc -> do
                                                                                    modify $ \s -> s { _pcode = s^.pcode ++ [rRun, rData x] }
                                                                                    compile
                                                                                Nothing -> do
                                                                                    y <- rData x
                                                                                    case y of
                                                                                        Integer i -> modify $ \s -> s { _pcode = s ^. pcode ++ [rPush, rData x] }
                                                                                        Float d -> modify $ \s -> s { _pcode = s ^. pcode ++ [rPush, rData x] }
                                                                                        Word w -> modify $ \s -> s { _pcode = s ^. pcode ++ [rRun, rData x] }
                                                                                    compile
        IST { _pcode = (x:xs) } -> do  
                                      s <- get
                                      if stackIsEmpty (s ^. cs) then exec
                                      else do
                                          c <- liftIO $ moreCode "... " 
                                          modify $ \s -> s { _program = c }
                                          compile

lexer :: String -> [ String ]
lexer input = words $ subRegex (mkRegex "#.*$") input "\n"

runInterpreter :: String -> IO ()
runInterpreter initCode = do
    (val, state) <- evalProg initCode
    print $ val 
