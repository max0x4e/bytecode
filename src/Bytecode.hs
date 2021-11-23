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
import Control.Lens ( set, view, over )
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
runtimeActions = fromList [("+", rOp (+)), ("-" , rOp (-)), ("/", rOp (/)), ("*", rOp (*)), ("over", rOver), ("dup", rDup), ("swap", rSwap), (".", rDot), 
            ("dump" , rDump), ("drop", rDrop), ("=", rComp (==)), (">", rComp (>)), ("<", rComp (<)), (",", rComa), ("@", rAt), ("!" , rBang), 
            ("allot", rAllot), ("create", rCreate), ("does>", rDoes)]

pop :: Stack Value -> Eval Value
pop s = do
    s <- gets s
    case stackPop ds of
        Just (s, x) -> return x
        Nothing -> liftIO $ die "Attempted to pop on empty stack [" ++ s ++ "]"

push :: Stack Value -> Value -> Eval Value
push s x = do 
    ist <- get
    set s x ist 
    put ist
    return Undef

peek :: Stack Value -> Eval Value
peek s = do
    s <- gets s
    case stackPeek s of
        Just x -> return x
        Nothing -> return Undef

rData :: String -> ByteCodeOp
rData w = case readMaybe w :: Maybe Int of
    Just i -> return (Integer i)
    Nothing -> case readMaybe w :: Maybe Float of 
        Just f -> return (Float f)
        Nothing -> return (Word w)

rOp :: ByteCodeOp
rOp op = do 
    ds <- gets _ds
    b <- pop ds
    a <- pop ds
    case (a,b) of 
        (Integer a', Integer b') -> push ds (Integer (op a' b'))
        (Float a', Float b') -> push ds (Float (op a' b'))
    return Undef

rOver :: ByteCodeOp
rOver = do
    ds <- gets _ds
    a <- pop ds
    b <- pop ds
    push ds b
    push ds a
    push ds b
    return Undef 

rDup :: ByteCodeOp
rDup = do
    ds <- gets _ds
    a <- pop ds
    push ds a
    push ds a
    return Undef

rSwap :: ByteCodeOp
rSwap = do
    ds <- gets _ds
    a <- pop ds
    b <- pop ds
    push ds a
    push ds b
    return Undef

rDot :: ByteCodeOp
rDot = do
    ds <- gets _ds
    a <- pop ds
    liftIO $ print a
    return Undef

rDump :: ByteCodeOp
rDump = do
    ds <- gets _ds
    liftIO $ print ds
    return Undef

rDrop :: ByteCodeOp
rDrop = gets _ds >>= pop >> return Undef

rJmp :: ByteCodeOp
rJmp = do
    ist <- get
    addr <- pcode ist !! pp ist
    case addr of
        Integer a' ->  put ist { pp = a' } >> return Undef
        _ -> liftIO $ die "Incorrect JUMP address."

rJnz :: ByteCodeOp
rJnz = do
    ist <- get
    addr <- pcode ist !! pp ist
    x <- pop ds ist
    case (x, addr) of 
        (Integer 0, _) -> put ist { _pp = _pp ist + 1 } >> return Undef
        (Integer _, Integer a') -> put ist { _pp = a' } >> return Undef

rJz :: ByteCodeOp
rJz = do
    ist <- get
    addr <- pcode ist !! pp ist
    x <- pop _ds ist
    case (x, addr) of 
        (Integer 0, Integer a') -> put ist { _pp = a' } >> return Undef
        _ -> put ist { _pp = _pp ist + 1 } >> return Undef

rComp :: ByteCodeOp
rComp op = do
    ds <- gets _ds
    b <- pop _ds
    a <- pop _ds
    case (a,b) of 
        (Integer a', Integer b') -> push ds (Integer (if op a' b' then 1 else 0))
        (Float a', Float b') -> push ds (Float (if op a' b' then 1 else 0))
    return Undef

rComa :: ByteCodeOp
rComa = get >>= put >> return Undef 

rAt :: ByteCodeOp
rAt = do
    ist <- get
    addr <- pop _ds ist
    case addr of
        Integer a -> push (_ds ist) (heap ist !! a)
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
    d <- _pcode ist !! _pp ist
    push (ds ist) d
    put ist { _pp = _pp ist + 1 }
    return Undef

exec :: Eval Value
exec = do
        ist <- get
        if pp ist == length (pcode ist) then compile
        else put ist { _pp = _pp ist + 1 } >> _pcode ist !! _pp ist >> exec

{-------------  Compile-time Actions ----------------------------}
compilerActions :: Dictionary ByteCodeOp
compilerActions = fromList [(":", cColon), (";", cSemi), ("if", cIf), ("else", cElse), ("then", cThen), ("begin", cBegin), ("until", cUntil)]

cColon :: ByteCodeOp
cColon = get >>= put >> return Undef
{- cColon = do
    ist <- get
    if stackIsEmpty (cs ist) then do
        case program ist of
            [] -> do
                c <- liftIO $ moreCode "... " 
                put ist { _cs = stackPush (_cs ist) (head c), _program = tail c }
                return Undef
            (x:xs) -> put ist { _cs = stackPush (_cs ist) x, _program = xs } >> return Undef     
    else liftIO $ die "Control Stack is not empty while trying to do : ." -}

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
        _pp = 0,
        _hp = 0,
        _ds = stackNew,
        _cs = stackNew,
        _heap = [],
        _program = lexer program,
        _pcode = [],
        _dd = empty
    } 
    in
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
            modify $ \ist -> ist { _program = c }
            compile
        IST { _program = (x:xs), _pcode = [] } -> if x == ":bye" 
                                                then do
                                                    liftIO $ putStrLn "Good bye!"
                                                    liftIO $ exitSuccess 
                                                else case M.lookup x compilerActions of
                                                         Just op -> op >> compile
                                                         Nothing -> case M.lookup x runtimeActions of 
                                                                        Just op -> do
                                                                            modify $ \ist -> ist { _pcode = _pcode ist ++ [op] }
                                                                            compile
                                                                        Nothing -> do
                                                                            dd <- gets _dd
                                                                            case M.lookup x dd of
                                                                                Just bc -> do
                                                                                    modify $ \ist -> ist { _pcode = _pcode ist ++ [rRun, rData x] }
                                                                                    compile
                                                                                Nothing -> do
                                                                                    y <- rData x
                                                                                    case y of
                                                                                        Integer i -> modify $ \ist -> ist { _pcode = _pcode ist ++ [rPush, rData x] }
                                                                                        Float d -> modify $ \ist -> ist { _pcode = _pcode ist ++ [rPush, rData x] }
                                                                                        Word w -> modify $ \ist -> ist { _pcode = _pcode ist ++ [rRun, rData x] }
                                                                                    compile
        IST { _pcode = (x:xs) } -> do 
                                      cs <- gets _cs
                                      if stackIsEmpty cs then exec
                                      else do
                                          c <- liftIO $ moreCode "... " 
                                          modify $ \ist -> ist { _program = c }
                                          compile

lexer :: String -> [ String ]
lexer input = words $ subRegex (mkRegex "#.*$") input "\n"

runInterpreter :: String -> IO ()
runInterpreter initCode = do
    (val, state) <- evalProg initCode
    print $ val 
