--Tyler Schulenberg
--Haskell Homework #5

-- Necessary imports
import Control.Applicative ((<$>),liftA,liftA2)
import Data.Map
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P


--------- AST Nodes ---------

-- Variables are identified by their name as string
type Variable = String

-- Values are either integers or booleans
data Value = IntVal Int       -- Integer value
           | BoolVal Bool     -- Boolean value

-- Expressions are variables, literal values, unary and binary operations
data Expression = Var Variable                    -- e.g. x
                | Val Value                       -- e.g. 2
                | BinOp Op Expression Expression  -- e.g. x + 3
                | Assignment Variable Expression  -- e.g. x = 3

-- Statements are expressions, conditionals, while loops and sequences
data Statement = Expr Expression                   -- e.g. x = 23
               | If Expression Statement Statement -- if e then s1 else s2 end
               | While Expression Statement        -- while e do s end
               | Sequence Statement Statement      -- s1; s2
               | Skip                              -- no-op
			   | For Variable Expression Expression Statement

-- All binary operations
data Op = Plus         --  +  :: Int -> Int -> Int
        | Minus        --  -  :: Int -> Int -> Int
        | Times        --  *  :: Int -> Int -> Int
        | GreaterThan  --  >  :: Int -> Int -> Bool
        | Equals       --  == :: Int -> Int -> Bool
        | LessThan     --  <  :: Int -> Int -> Bool

-- The `Store` is an associative map from `Variable` to `Value` representing the memory
type Store = Map Variable Value

--------- Parser ---------

-- The Lexer

lexer = P.makeTokenParser (emptyDef {
  P.identStart = letter,
  P.identLetter = alphaNum,
  P.reservedOpNames = ["+", "-", "*", "!", ">", "=", "==", "<"],
  P.reservedNames = ["true", "false", "if", "in", "then", "else", "while", "end", "to", "do", "for"]
})

-- The Parser

-- Number literals
numberParser :: Parser Value
numberParser = (IntVal . fromIntegral) <$> P.natural lexer

-- Boolean literals
boolParser :: Parser Value
boolParser =  (P.reserved lexer "true" >> return (BoolVal True))
          <|> (P.reserved lexer "false" >> return (BoolVal False))

-- Literals and Variables
valueParser :: Parser Expression
valueParser =  Val <$> (numberParser <|> boolParser)
           <|> Var <$> P.identifier lexer
		   
-- Used piazza discussions to help set up for parser		   
variableParser :: Parser Variable
variableParser = P.identifier lexer

-- -- Expressions
exprParser :: Parser Expression
exprParser = liftA2 Assignment
                    (try (P.identifier lexer >>= (\v ->
                          P.reservedOp lexer "=" >> return v)))
                    exprParser
          <|> buildExpressionParser table valueParser
    where table = [[Infix (op "*" (BinOp Times)) AssocLeft]
                  ,[Infix (op "+" (BinOp Plus)) AssocLeft]
                  ,[Infix (op "-" (BinOp Minus)) AssocLeft]
                  ,[Infix (op ">" (BinOp GreaterThan)) AssocLeft]
                  ,[Infix (op "==" (BinOp Equals)) AssocLeft]
                  ,[Infix (op "<" (BinOp LessThan)) AssocLeft]]
          op name node = (P.reservedOp lexer name) >> return node

-- Sequence of statements
stmtParser :: Parser Statement
stmtParser = stmtParser1 `chainl1` (P.semi lexer >> return Sequence)

-- Single statements
stmtParser1 :: Parser Statement
stmtParser1 = (Expr <$> exprParser)
          <|> do
              P.reserved lexer "if"
              cond <- exprParser
              P.reserved lexer "then"
              the <- stmtParser
              P.reserved lexer "else"
              els <- stmtParser
              P.reserved lexer "end"
              return (If cond the els)
          <|> do
              P.reserved lexer "while"
              cond <- exprParser
              P.reserved lexer "do"
              body <- stmtParser
              P.reserved lexer "end"
              return (While cond body)
		  <|> do
			  P.reserved lexer "for"
			  iter <- variableParser
			  P.reserved lexer "in"
			  lower <- exprParser
			  P.reserved lexer "to"
			  upper <- exprParser
			  P.reserved lexer "do"
			  body <- stmtParser
			  P.reserved lexer "end"
			  return (For iter lower upper body)

-------- Helper functions --------

-- Lift primitive operations on IntVal and BoolVal values
liftIII :: (Int -> Int -> Int) -> Value -> Value -> Value
liftIII f (IntVal x) (IntVal y) = IntVal $ f x y
liftIIB :: (Int -> Int -> Bool) -> Value -> Value -> Value
liftIIB f (IntVal x) (IntVal y) = BoolVal $ f x y

-- Apply the correct primitive operator for the given Op value
applyOp :: Op -> Value -> Value -> Value
applyOp Plus        = liftIII (+)
applyOp Minus       = liftIII (-)
applyOp Times       = liftIII (*)
applyOp GreaterThan = liftIIB (>)
applyOp Equals      = liftIIB (==)
applyOp LessThan    = liftIIB (<)

-- Parse and print (pp) the given WHILE programs
pp :: String -> IO ()
pp input = case (parse stmtParser "" input) of
    Left err -> print err
    Right x  -> print x

-- Parse and run the given WHILE programs
run :: (Show v) => (Parser n) -> String -> (n -> Store -> v) -> IO ()
run parser input eval = case (parse parser "" input) of
    Left err -> print err
    Right x  -> print (eval x empty)

-- 1
-- First, make all abstract syntax tree (AST) node types instances of the Show type class. You will have to implement the show function such that showing statements and expressions yields code that would be accepted by the parser.
instance Show Statement where
	show (Expr e) = show e
	show (If e s1 s2) = "if " ++ show e ++ " then " ++ show s1 ++ " else " ++ show s2 ++ " end "
	show (While e s) = "while " ++ show e ++ " do " ++ show s ++ " end "
	show (Sequence s1 s2) = show s1 ++ ";" ++ show s2
	show (Skip) = error " skip error"
	show (For v e1 e2 s) = "for " ++ v ++ " in " ++ show e1 ++ " to " ++ show e2 ++ " do " ++ show s ++ " end"
instance Show Value where
	show (IntVal i) = show i -- e.g. x
	show (BoolVal b)         -- e.g. 2
		| b == True = "true"  -- e.g. x + 3
		| b == False = "false" -- e.g. x = 3

instance Show Op where
	show (Plus) = " + "
	show (Minus) = " - "
	show (Times) = " * "
	show (GreaterThan) = " > "
	show (Equals) = " == "
	show (LessThan) = " < "

instance Show Expression where
--  look up name x from store s
	show (Var v) = v
	show (Val va) = show va
	show (BinOp o e1 e2) = show e1 ++ show o ++ show e2 
	show (Assignment v e1)= v ++ " = " ++ show e1

-- 2
-- Write a function
-- evalE :: Expression -> Store -> (Value, Store)
-- that takes as input an expression and a store and returns a value. If a variable is not found (e.g. because it is not initialized) throw an error with the error function.

evalE :: Expression -> Store -> (Value, Store)
evalE (BinOp o a b) s = (applyOp o a' b', s'')
    where (a', s')  = evalE a s
          (b', s'') = evalE b s'
-- lookup name x from store s which may give value to return as first part of result. 2nd result is just s	  
evalE (Var x) s = (x', s)
	where x' = case (Data.Map.lookup x s) of 
				Just a -> a
				Nothing -> error "error var"
evalE (Val v) s = (v, s)
	where s' = Data.Map.insert (show v) v s			
-- returns a value and a store
-- evaluate expression e to return value and potentially modified store
-- use x as s key and value
evalE (Assignment x e) s = (x', s'')
	where (x', s') = evalE e s
	      s'' = Data.Map.insert x x' s'
		  
-- 3 
-- Next, write a function
-- evalS :: Statement -> Store -> Store
-- that takes as input a statement and a store and returns a possibly modified store

evalS :: Statement -> Store -> Store
evalS w@(While e s1) s = case (evalE e s) of
                          (BoolVal True,s')  -> let s'' = evalS s1 s' in evalS w s''
                          (BoolVal False,s') -> s'
                          _                  -> error "Condition must be a BoolVal"		  
evalS Skip s = s
evalS (Expr e) s = s'
	where (x', s') = evalE e s

evalS (Sequence s1 s2) s = evalS s2 (evalS s1 s)
evalS (If e s1 s2) s = case (evalE e s) of
						(BoolVal True,s') -> evalS s1 s'
						(BoolVal False,s') -> evalS s2 s'
						_ -> error "Condition must be a BoolVal"
evalS (For v e1 e2 s1) s = case (evalE (BinOp LessThan e1 e2) s') of
							(BoolVal True,s'') -> evalS (For v (BinOp Plus e1 (Val (IntVal 1))) e2 s1) (evalS s1 s') 
							(BoolVal False,s'') -> s
							_ -> error "fail safe"
	where 
		(v',s') = evalE (Assignment v e1) s


--For Variable Expression Expression Statement
--var will be set to an initial value given by whatever the expression e1\
-- returns and will execute the body s repeatedly, increasing the loop variable after each iteration,
-- until the variable var is greater than the value returned by expression e2.
						
-- 4	
-- We use the Maybe type to deal with cases like uninitialized variables or non-boolean tests instead of throwing a runtime error.
-- Write the following function that takes an expression and store and returns a Maybe (Value,Store) and never throws a runtime error.
					
evalE_maybe :: Expression -> Store -> Maybe (Value, Store)
evalE_maybe (BinOp o a b) s = do (a',s') <- evalE_maybe a s
                                 (b',s'') <- evalE_maybe b s'
                                 return (applyOp o a' b', s'')
evalE_maybe (Var x) s = case (Data.Map.lookup x s) of 
				Just a -> Just (a,s)
				Nothing -> Nothing
evalE_maybe (Val v) s = Just (v, s)
	where s' = Data.Map.insert (show v) v s	
evalE_maybe (Assignment x e) s = Just (x', s'')
	where Just (x', s') = evalE_maybe e s
	      s'' = Data.Map.insert x x' s'
					
evalS_maybe :: Statement -> Store -> Maybe Store
evalS_maybe w@(While e s1) s = case (evalE e s) of
                        (BoolVal True,s')  -> let s'' = evalS s1 s' in evalS_maybe w s''
                        (BoolVal False,s') -> Just s'
                        _                  -> Nothing
evalS_maybe Skip s = Just s
evalS_maybe (Sequence s1 s2) s = evalS_maybe s2 (evalS s1 s)
evalS_maybe (Expr e) s = Just s'
	where (x', s') = evalE e s
evalS_maybe (If e s1 s2) s = case (evalE e s) of
						(BoolVal True,s') -> evalS_maybe s1 s'
						(BoolVal False,s') -> evalS_maybe s2 s'
						_ -> Nothing
--evalS_maybe (For v e1 e2 s1) s = ...
-------------------------------------------

-- 5
newtype Imperative a = Imperative {
    runImperative :: Store -> Maybe (a, Store)
}

instance Monad Imperative where
    return a = Imperative (\s -> Just (a,s))
    b >>= f = Imperative (\s -> do (v1,s1) <- (runImperative b) s
                                   runImperative (f v1) s1)
    fail _ = Imperative (\s -> Nothing)
	
evalE_monad :: Expression -> Imperative Value
evalE_monad (BinOp o a b) = do a' <- evalE_monad a
                               b' <- evalE_monad b
                               return (applyOp o a' b')
evalE_monad (Var x) = getVar x
evalE_monad (Val v) = return v		
evalE_monad (Assignment x e) = do 
							e' <- evalE_monad e
							x' <- setVar x e'
							return x'

evalS_monad :: Statement -> Imperative ()
evalS_monad (While e s1) = do
						e' <- evalE_monad e
						case (e') of
							(BoolVal True)  -> evalS_monad s1
						return ()
evalS_monad Skip = return ()
evalS_monad (Sequence s1 s2) = do
							s1' <- evalS_monad s1
							s2' <- evalS_monad s2
							return ()
evalS_monad (Expr e) = do
						e' <- evalE_monad e
						return ()
evalS_monad (If e s1 s2) = do
						e' <- evalE_monad e
						case (e') of
							(BoolVal True) -> evalS_monad s1
							(BoolVal False) -> evalS_monad s2
						return ()
--evalS_monad (For v e1 e2 s1) s = ...

-- Parse and run the given WHILE programs using monads
runMonad :: String -> Maybe Store
runMonad input = proc (parse stmtParser "" input)
    where proc (Right x) = snd `fmap` runImperative (evalS_monad x) empty
          proc _         = Nothing

getVar :: Variable -> Imperative Value
getVar var = Imperative (\store -> ((Data.Map.lookup var store) >>= (\v -> Just (v,store))))

setVar :: Variable -> Value -> Imperative Value
setVar var val = Imperative (\store -> Just (val, Data.Map.insert var val store)) 

miniprog :: Imperative Value
miniprog = do
            setVar "x" (IntVal 2)
            setVar "y" (IntVal 3)
            a <- getVar "x"
            b <- getVar "y"
            return (applyOp Plus a b)