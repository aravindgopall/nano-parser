{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module NanoParser where

import           Prelude
import Debug.Trace

---- Parser Type -----
data Parser s a =
  Parser
    { runParser :: s -> [(a, s)]
    }

------ String Parser -----
type ParserS a = Parser String a

runS :: Show a => ParserS a -> String -> a
runS (Parser p) s =
  case p s of
    [(res, _)] -> res
    [(_, x)]   -> error "still left: "
    x          -> error $ "parse error" <> show x

------------------------
char :: ParserS Char
char =
  Parser $ \s ->
    case s of
      []     -> []
      (x:xs) -> [(x, xs)]

-------- Monad ---------
bind :: ParserS a -> (a -> ParserS b) -> ParserS b
bind p b =
  Parser $ \s -> concatMap (\(a, s') -> runParser (b a) s') $ runParser p s

unit :: a -> ParserS a
unit a = Parser $ \s -> [(a, s)]

-------- Functor --------
instance Functor (Parser String)
  --fmap f p = Parser $ \s -> map (\(c,s') -> (f c, s')) $ runParser p s
                                                                         where
  fmap f p = Parser (\s -> [(f c, s') | (c, s') <- runParser p s])

instance Applicative (Parser String) where
  pure = unit
  (Parser p1) <*> (Parser p2) =
    Parser (\s -> [(f a, s2) | (f, s1) <- p1 s, (a, s2) <- p2 s1])

instance Monad (Parser String) where
  return = unit
  (>>=) = bind

------------------- Monoid ----------
append :: Monoid a => ParserS a -> ParserS a -> ParserS a
append p1 p2 =
  Parser (\s -> [(a <> b, s1 <> s2) | (a, s1) <- runParser p1 s, (b, s2) <- runParser p2 s])

failure :: ParserS a
failure = Parser $ const []

combine :: ParserS a -> ParserS a -> ParserS a
combine p1 p2 = Parser $ \s -> runParser p1 s ++ runParser p2 s

option :: ParserS a -> ParserS a -> ParserS a
option p1 p2 =
  Parser $ \s ->
    case runParser p1 s of
      [] -> runParser p2 s
      res -> res

---------- many, some ---------
-- zero or more
many :: ParserS a -> ParserS [a]
many v =
  let many_v =
        option ((:) <$> v <*> many_v) (pure [])
    in many_v

-- one or more
some :: ParserS a -> ParserS [a]
some v =
  let some_v =
        (:) <$> v <*> (option some_v (pure []))
   in some_v


------- predicates -------
satisfy :: (Char -> Bool) -> ParserS Char
satisfy f =
  char >>= (\c ->
    if f c
       then unit c
       else Parser $ const [])

oneOf :: [Char] -> ParserS Char
oneOf chs = satisfy (flip elem chs)

---- run one or more based on the predicate with default value
-- zero or more
chain :: ParserS a -> ParserS (a -> a -> a) -> a -> ParserS a
chain p1 pf a = option (chain1 p1 pf) (return a)

-- one or more
chain1 :: ParserS a -> ParserS (a -> a -> a) -> ParserS a
chain1 p1 pf = do
  a <- p1
  let runRest x =
        option (do
          f <- pf
          b <- p1
          runRest (f x b)) (return x)
   in runRest a


---- Predicates ------------
--
isDigit :: Char -> Bool
isDigit = (flip elem ['0' ..'9'])

number :: ParserS Integer
number =  read <$> some (satisfy isDigit)

string :: String -> ParserS String
string [] = return []
string (x:xs) = do
  c <- satisfy (x ==)
  cs <- string xs
  return(c: cs)

digit :: ParserS Char
digit = satisfy isDigit

integer :: ParserS Int
integer = do
  s <- option (string "-") (return [])
  c <- some digit
  return $ read $ s ++ c

spaces :: ParserS String
spaces = many (satisfy (' ' ==))

token :: ParserS a -> ParserS a
token p = do { a <- p; spaces; return a}

reserved :: String -> ParserS String
reserved s = token (string s)

parens :: ParserS a -> ParserS a
parens m = do
  reserved "("
  y <- m
  reserved ")"
  return y

-------------------- Bacus-Naur Form Language ---------
--
data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Lit Int
  deriving (Show)

eval :: Expr -> Int
eval (Add a b) = eval a + eval b
eval (Sub a b) = eval a - eval b
eval (Mul a b) = eval a * eval b
eval (Lit a) = a

lit :: ParserS Expr
lit = do
  n <- integer
  return $ Lit n

infixOp :: String -> (a -> a -> a) -> ParserS (a -> a -> a)
infixOp x f = reserved x >> return f

addop :: ParserS (Expr -> Expr -> Expr)
addop = (infixOp "+" Add)

subop :: ParserS (Expr -> Expr -> Expr)
subop = infixOp "-" Sub

mulop :: ParserS (Expr -> Expr -> Expr)
mulop = infixOp "*" Mul

rec :: ParserS Expr
rec = option lit (parens expr)

expr :: ParserS Expr
expr = rec `chain1` (option addop (option mulop subop))

run :: String -> Expr
run = runS expr

