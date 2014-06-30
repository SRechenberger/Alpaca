{-|
Module      : Alpaca.MaMa.Machine
Description : Alpaca Machine
Maintainer  : sascha.rechenberger@uni-ulm.de
Stability   : stable
Portability : portable
Copyright   : (c) Sascha Rechenberger, 2014
License     : GPL-3

This file is part of Alpaca.

Alpaca is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Alpaca is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Alpaca. If not, see <http://www.gnu.org/licenses/>.
-}

module Alpaca.MaMa.Machine
(
    -- * Simple Types
    Pointer, Program,

    -- * Monad
    MaMa, stmt, haul, runProgram,

    -- * error
    runtimeError
)
where

    import Prelude hiding (lookup)

    import Alpaca.MaMa.Types (Basic (..), HeapObject (..))

    import Control.Monad.Trans.Either (EitherT, runEitherT, left)
    import Control.Monad.State.Lazy (State, runState, get, put, execState, evalState)
    import Control.Applicative ((<$>), (<*>), (*>), (<*), pure)

    import Data.Map (Map, insert, lookup, member, adjust, empty, (!), size, fromList)
    import Data.List (intercalate)

    import Debug.Trace (trace)

    import Text.Parsec (parse, try, many1, char, string, lower, (<|>), lookAhead, spaces, unexpected, oneOf, many1, anyChar, digit, sepBy1)
    import Text.Parsec.String (Parser)

    type Pointer = Int
    type Heap = (Pointer, Map Pointer HeapObject)
    type Stack = Map Pointer Basic
    type Program = Map Pointer (MaMa ())
    data MaMaState = MaMaState {pc :: Pointer, sp :: Pointer, fp :: Pointer, gp :: Pointer, st :: Stack, hp :: Heap} deriving(Show)

    data Result = Success Basic | Failure String deriving (Show)

    type MaMa = EitherT Result (State MaMaState)

    data Address = PC | SP | FP | GP 
        | ST Address 
        | HP Address Selector 
        | Const Pointer 
        | New Tag [Address]
        | Unary (Pointer -> Pointer) Address 
        | Binary (Pointer -> Pointer -> Pointer) Address Address
        deriving ()

    instance Show Address where
        show PC = "$pc"
        show SP = "$sp"
        show FP = "$fp"
        show GP = "$gp"
        show (ST a) = "$st[" ++ show a ++ "]"
        show (HP a s) = "$hp[" ++ show a ++ "]." ++ show s
        show (Const x) = show x
        show (New t as) = "new " ++ show t ++ " (" ++ intercalate ", " (map show as) ++ ")"
        show (Unary f a) = case f 1 of
            -1 -> "(-" ++ show a ++ ")"
            _  -> "f(" ++ show a ++ ")"
        show (Binary f a b) = case f (-2) (-2) of
            -4 -> "(" ++ show a ++ " + " ++ show b ++ ")"
            0  -> "(" ++ show a ++ " - " ++ show b ++ ")"
            4  -> "(" ++ show a ++ " * " ++ show b ++ ")"
            1  -> "(" ++ show a ++ " / " ++ show b ++ ")"
            _  -> "f(" ++ show a ++ ", " ++ show b ++ ")"

    data Tag = BASIC | CLOSURE | FUNVAL | VECTOR deriving (Eq, Show)

    data Selector = V | A Int | CP | GLOB | AP | N deriving (Eq)

    instance Show Selector where
        show V = "b"
        show (A i) = "v[" ++ show i ++ "]"
        show CP = "cp"
        show GLOB = "gp"
        show AP = "ap"
        show N = "n"

    data Assign = Address := Address deriving (Show)

    runtimeError :: String -> MaMa a
    runtimeError s = left . Failure $ "RUNTIME ERROR " ++ s ++ "."

    getLabel :: Address -> MaMa Pointer 
    getLabel (Const ptr) = return ptr
    getLabel (Unary f a) = f <$> getLabel a 
    getLabel (Binary f a b) = f <$> getLabel a <*> getLabel b
    getLabel a = do 
        ptr <- rightHandSide a 
        case ptr of 
            Label p -> return p 
            _       -> runtimeError $ show ptr ++ " is no label"

    getPointer :: Address -> MaMa Pointer 
    getPointer (Const ptr) = return ptr
    getPointer (Unary f a) = f <$> getPointer a 
    getPointer (Binary f a b) = f <$> getPointer a <*> getPointer b
    getPointer a = do 
        ptr <- rightHandSide a 
        case ptr of 
            Pointer p -> return p 
            _         -> runtimeError $ show ptr ++ " is no pointer"


    rightHandSide :: Address -> MaMa Basic
    rightHandSide a = case a of 
        PC -> get >>= return . Label . pc 
        SP -> get >>= return . Pointer . sp 
        FP -> get >>= return . Pointer . fp 
        GP -> get >>= return . Pointer . gp
        New tag args -> case tag of
            BASIC -> if length args /= 1 
                then runtimeError $ "BASIC object needing 1 argument misdistributed with " ++ show (length args)
                else return args >>= rightHandSide . head >>= new . Basic
            CLOSURE -> if length args /= 2 
                then runtimeError $ "CLOSURE object needing 2 arguments misdistributed with " ++ show (length args)
                else do
                    cp <- getLabel (args !! 0)
                    gp <- getPointer (args !! 1)
                    new $ Closure cp gp 
            FUNVAL -> if length args /= 3
                then runtimeError $ "FUNVAL object needing 3 arguments misdistributed with " ++ show (length args)
                else do
                    cp <- getLabel (args !! 0)
                    ap <- getPointer (args !! 1)
                    gp <- getPointer (args !! 2)
                    new $ FunVal cp ap gp 
            VECTOR -> if length args /= 1 
                then runtimeError $ "VECTOR object needing 1 argument misdistributed with " ++ show (length args)
                else pure args >>= getPointer . head >>= new . flip Vector empty
        ST p -> getPointer p >>= \p -> get >>= \mama -> case lookup p (st mama) of
            Nothing   -> runtimeError $ "cannot read ST[" ++ show p ++ "]; null pointer"
            Just elem -> return elem
        HP p sel -> getPointer p >>= \p -> get >>= \mama -> case lookup p (snd . hp $ mama) of
            Nothing -> runtimeError $ "cannot read HP[" ++ show p ++ "]; null pointer"
            Just elem -> case elem of
                Basic b -> case sel of 
                    V -> return b
                    _  -> runtimeError $ "cannot get " ++ show sel ++ " of a basic"
                Closure lbl glob -> case sel of
                    CP -> return . Label $ lbl 
                    GLOB -> return . Pointer $ glob
                    _  -> runtimeError $ "cannot get " ++ show sel ++ " of a closure"
                FunVal lbl fap glob -> case sel of 
                    CP -> return . Label $ lbl
                    AP -> return . Pointer $ fap
                    GLOB -> return . Pointer $ glob
                    _  -> runtimeError $ "cannot get " ++ show sel ++ " of a function"
                Vector n vals -> case sel of
                    A ix -> if ix >= size vals 
                        then runtimeError $ show ix ++ " violates vector bounds"
                        else return . Pointer $ vals ! ix
                    N -> return . Pointer $ n
                    _  -> runtimeError $ "cannot get " ++ show sel ++ " of a vector"
                Cons _ n vals -> case sel of 
                    A ix -> if ix >= size vals 
                        then runtimeError $ show ix ++ " violates constructor bounds"
                        else return . Pointer $ vals ! ix
                    _  -> runtimeError $ "cannot get " ++ show sel ++ " of a constructor"
        err -> runtimeError $ show err ++ " is no legal right hand side"
        {-}
        Unary f a -> (Pointer . f) <$> getPointer a
        Binary f a b -> Pointer <$> (f <$> getPointer a <*> getPointer b) 
        Const x   -> return . Pointer $ x -}

    assign :: Assign -> MaMa ()
    assign (lhs := rhs) = do
        case lhs of
            PC -> getLabel rhs >>= \lbl -> get >>= \mama -> put mama {pc = lbl}
            SP -> getPointer rhs >>= \ptr -> get >>= \mama -> put mama {pc = ptr}
            FP -> getPointer rhs >>= \ptr -> get >>= \mama -> put mama {pc = ptr}
            GP -> getPointer rhs >>= \ptr -> get >>= \mama -> put mama {pc = ptr}
            ST a -> rightHandSide rhs >>= \datum -> getPointer a >>= \ptr -> get >>= \mama -> if ptr `member` st mama 
                then put mama {st = adjust (const datum) ptr (st mama)}
                else put mama {st = insert ptr datum (st mama)}
            HP p sel -> getPointer p >>= \ptr -> get >>= \mama -> case lookup ptr (snd . hp $ mama) of
                Nothing -> runtimeError $ "cannot update HP[" ++ show p ++ "]; null pointer"
                Just elem -> case elem of 
                    Basic _ -> rightHandSide rhs >>= \datum -> case sel of 
                        V -> put mama {hp = (fst . hp $ mama, adjust (const . Basic $ datum) ptr . snd . hp $ mama)}
                        e -> runtimeError $ "cannot write " ++ show e ++ " field of a BASIC object"
                    Closure c v -> case sel of
                        CP -> getLabel rhs >>= \lbl -> get >>= \mama -> put mama {hp = (fst . hp $ mama, adjust (const (Closure lbl v)) ptr . snd . hp $ mama)}
                        GLOB -> rightHandSide rhs >>= \datum -> case datum of
                            Pointer x -> put mama {hp = (fst . hp $ mama, adjust (const (Closure c x)) ptr . snd . hp $ mama)}
                            err       -> runtimeError $ "cannot write " ++ show err ++ " into the gp field of a closure"
                        e  -> runtimeError $ "cannot write " ++ show e ++ " field of a CLOSURE object"
                    FunVal c a v -> case sel of
                        CP -> rightHandSide rhs >>= \datum -> case datum of
                            Label x -> put mama {hp = (fst . hp $ mama, adjust (const (FunVal x a v)) ptr . snd . hp $ mama)}
                            err     -> runtimeError $ "cannot write " ++ show err ++ " into the cp field of a funval"
                        AP -> rightHandSide rhs >>= \datum -> case datum of
                            Pointer x -> put mama {hp = (fst . hp $ mama, adjust (const (FunVal c x v)) ptr . snd . hp $ mama)}
                            err       -> runtimeError $ "cannot write " ++ show err ++ " into the ap field of a funval"
                        GLOB -> rightHandSide rhs >>= \datum -> case datum of
                            Pointer x -> put mama {hp = (fst . hp $ mama, adjust (const (FunVal c a x)) ptr . snd . hp $ mama)}
                            err       -> runtimeError $ "cannot write " ++ show err ++ " into the gp field of a funval"
                        e  -> runtimeError $ "cannot write " ++ show e ++ " field of a FUNVAL object"
                    Vector n ptrs -> case sel of
                        A ix -> rightHandSide rhs >>= \datum -> case datum of
                            Pointer x -> put mama {hp = (fst . hp $ mama, adjust (const (Vector n (adjust (const x) ix ptrs))) ptr . snd . hp $ mama)}
                            err       -> runtimeError $ "cannot write " ++ show err ++ " into the a field of a vector"
                        N -> rightHandSide rhs >>= \datum -> case datum of
                            Pointer x -> put mama {hp = (fst . hp $ mama, adjust (const (Vector x ptrs)) ptr . snd . hp $ mama)}
                            err       -> runtimeError $ "cannot write " ++ show err ++ " into the a field of a vector"
                        e  -> runtimeError $ "cannot write " ++ show e ++ " field of a VECTOR object"
            _ -> runtimeError $ "no memory cell is chosen"


    new :: HeapObject -> MaMa Basic
    new obj = do
        mama <- get
        let next = fst . hp $ mama
        let heap = snd . hp $ mama
        put mama {hp = (next + 1, insert next obj heap)}
        return . Pointer $ next

    assignment :: Map String Basic -> Parser Assign 
    assignment env = (:=) <$> location env <*> (spaces *> string ":=" *> spaces *> location env)

    location :: Map String Basic -> Parser Address
    location env = try memory <|> try new <|> try param
        where
            memory, param, new :: Parser Address
            memory = do 
                reg <- char '$' *> many1 lower 
                case reg of 
                    "sp" -> return SP 
                    "pc" -> return PC 
                    "gp" -> return GP 
                    "fp" -> return FP
                    "st" -> ST <$> (char '[' *> param <* char ']')
                    "hp" -> HP <$> (char '[' *> param <* char ']') <*> (char '.' *> selector)  
                    s    -> unexpected $ "$" ++ s ++ " is no memory"
            new = do 
                string "new"
                spaces
                x <- oneOf "BCFV"
                spaces 
                char '('
                params <- param `sepBy1` (do spaces; char ','; spaces)
                char ')'
                case x of 
                    'B' -> return $ New BASIC params
                    'C' -> return $ New CLOSURE params
                    'F' -> return $ New FUNVAL params
                    'V' -> return $ New VECTOR params
            selector :: Parser Selector
            selector = do
                s <- many1 lower
                case s of
                    "b" -> return V
                    "v" -> A <$> (char '[' *> (try var <|> try const) <* char ']')
                    "gp" -> return GLOB
                    "cp" -> return CP 
                    "ap" -> return AP 
                    "n"  -> return N
                where
                    var, const :: Parser Int 
                    var = do
                        x <- many1 lower
                        case lookup x env of
                            Nothing          -> unexpected $ x ++ " was never bound"
                            Just (Pointer x) -> return x
                            Just (Label x)   -> return x
                            Just err         -> unexpected $ show err ++ " is no pointer"
                    const = read <$> many1 digit


            param = try (char '(' *> unary <* char ')') <|> try (char '(' *> binary <* char ')') <|> try const <|> try memory <|> try var
                where
                    unary, binary, const, var :: Parser Address
                    unary = char '-' *> (Unary negate <$> memory)

                    const = Const . read <$> many1 digit

                    var = do
                        v <- many1 lower
                        case lookup v env of
                            Nothing          -> unexpected $ v ++ " was never bound"
                            Just (Pointer x) -> return . Const $ x
                            Just (Label x)   -> return . Const $ x
                            Just err         -> unexpected $ show err ++ " is no pointer"

                    binary = do
                        a <- try param
                        spaces
                        o <- op 
                        spaces
                        b <- try param
                        return (Binary o a b)
                        where
                            op :: Parser (Pointer -> Pointer -> Pointer)
                            op = do 
                                o <- oneOf "+-*/"
                                case o of
                                    '+' -> return (+)
                                    '-' -> return (-)
                                    '*' -> return (*)
                                    '/' -> return div 
                                    err -> unexpected $ err:" is no binary operator"



    stmt :: [(String, Basic)] -> String -> MaMa ()
    stmt env input = case parse (assignment (fromList env)) "" input of
        Right a -> assign a 
        Left  s -> runtimeError $ "CODE ERROR " ++ intercalate " " (words . show $ s)

    haul :: [(String, Basic)] -> String -> MaMa Basic 
    haul env input = case parse (location (fromList env)) "" input of
        Right a -> rightHandSide a 
        Left  s -> runtimeError $ "CODE ERROR " ++ intercalate " " (words . show $ s)

--    runProgram :: Map Pointer (MaMa ()) -> Result 
    
    mainLoop prg = do
        pc <- haul [] "$pc"
        case pc of
            Label lbl -> do
                stmt [] "$pc := $pc + 1"
                case lookup lbl prg of
                    Nothing -> runtimeError $ "line " ++ show lbl ++ " does no exist"
                    Just a  -> a 
            _         -> runtimeError $ show pc ++ " is no label"
        mainLoop prg

    runProgram :: Program -> Either Result ()
    runProgram prg = flip evalState (MaMaState 0 0 0 0 empty (0, empty)) . runEitherT $ mainLoop prg

    test = flip runState (MaMaState 0 0 0 0 empty (0, empty)) . runEitherT $ do
        stmt [("k", Pointer 10), ("g", Pointer 5)] "$pc := 1 + g"