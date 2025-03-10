-- Part 2
import qualified Text.Parsec as Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Expr (buildExpressionParser)
import Text.Parsec.Expr( Operator(..), Assoc(..) )
import Control.Monad (liftM)
import qualified Text.Parsec.Token as Token
import Data.List (isInfixOf)

import Stack
import State
import LowLevelMachine as LLM

languageDef =
  emptyDef { 
              Token.identStart      = Parsec.letter,
              Token.identLetter     = Parsec.alphaNum,  
              Token.reservedNames   = ["if", "then", "else", "while", "do", "not", "and", "True", "False"],
              Token.reservedOpNames = ["+", "-", "*", ":=", "==", "<=", "and", "not"]
           }

lexer = Token.makeTokenParser languageDef

identifier = Parsec.try $ do
    name <- Token.identifier lexer
    if any (`isInfixOf` name) (Token.reservedNames languageDef)
        then error $ "Variable name " ++ show name ++ " contains a reserved keyword!"
        else return name
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
integer    = Token.integer    lexer
semi       = Token.semi       lexer
whiteSpace = Token.whiteSpace lexer

-- Aexp: Arithmetic Expressions
data Aexp = Num Integer
            | Var String
            | Add Aexp Aexp
            | Mult Aexp Aexp
            | Sub Aexp Aexp
            deriving (Eq, Show)

-- Bexp: Boolean Expressions
data Bexp = TT
            | FF
            | Neg Bexp
            | And Bexp Bexp
            | Equ Aexp Aexp
            | EquB Bexp Bexp
            | Le Aexp Aexp
            deriving (Eq, Show)

-- Stm: Statements
data Stm = Assign String Aexp
            | Seq Stm Stm
            | If Bexp [Stm] [Stm]
            | While Bexp [Stm]
            deriving (Eq, Show)

-- Compile Arithmetic Expressions
compA :: Aexp -> Code
compA (Num n) = [Push n]
compA (Var x) = [Fetch x]
compA (Main.Add a1 a2) = compA a1 ++ compA a2 ++ [LLM.Add] 
compA (Main.Mult a1 a2) = compA a1 ++ compA a2 ++ [LLM.Mult] 
compA (Main.Sub a1 a2) = compA a2 ++ compA a1 ++ [LLM.Sub]  

-- Compile Boolean Expressions
compB :: Bexp -> Code
compB Main.TT = [LLM.Tru]
compB Main.FF = [LLM.Fals]
compB (Main.Neg b) = compB b ++ [LLM.Neg]
compB (Main.And b1 b2) = compB b1 ++ compB b2 ++ [LLM.And]
compB (Main.Equ a1 a2) = compA a1 ++ compA a2 ++ [LLM.Equ]
compB (Main.EquB b1 b2) = compB b1 ++ compB b2 ++ [LLM.Equ]
compB (Main.Le a1 a2) = compA a2 ++ compA a1 ++ [LLM.Le]

-- Compile Statements
compStm :: Stm -> Code
compStm (Assign x a) = compA a ++ [LLM.Store x]
compStm (Seq s1 s2) = compStm s1 ++ compStm s2
compStm (If b s1 s2) = compB b ++ [LLM.Branch (compile  s1) (compile s2)]
compStm (While b s) =  [LLM.Loop (compB b) (compile s)]


-- Compile Programs
compile :: [Stm] -> Code
compile [] = []
compile (s:ss) = compStm s ++ compile ss



-- Precedence and associativity of arithmetic operators
aOperators = [ 
              [Infix (reservedOp "*" >> return Main.Mult) AssocLeft], 
              [Infix (reservedOp "+" >> return Main.Add) AssocLeft],
              [Infix (reservedOp "-" >> return Main.Sub) AssocLeft]
             ]

-- Term parser for arithmetic expressions
aTerm = parens aexp
     Parsec.<|> liftM Var identifier
     Parsec.<|> liftM Num integer

-- Arithmetic expression parser
aexp = buildExpressionParser aOperators aTerm


-- Parser for Less than or Equal to
comparison :: Parser Bexp
comparison = do
  a1 <- aexp
  op <- reservedOp "<=" >> return Main.Le
  a2 <- aexp
  return (op a1 a2)

-- Parser for == (equality)
equality :: Parser Bexp
equality = do
  a1 <- aexp
  op <- reservedOp "==" >> return Main.Equ
  a2 <- aexp
  return (op a1 a2)

-- Precedence and associativity of boolean operators
bOperators = [ 
              [Prefix (reservedOp "not" >> return Main.Neg)],
              [Infix (reservedOp "=" >> return Main.EquB) AssocLeft],
              [Infix (reservedOp "and" >> return Main.And) AssocLeft]
             ]

-- Term parser for boolean expressions
bTerm = parens bexp
       Parsec.<|> Parsec.try equality
       Parsec.<|> (reserved "True" >> return Main.TT)
       Parsec.<|> (reserved "False" >> return Main.FF)
       Parsec.<|> Parsec.try comparison
       
     
-- Boolean expression parser
bexp = buildExpressionParser bOperators bTerm

-- Parser for a list of statements
stms :: Parser [Stm]
stms = Parsec.many stm

-- Statement parser
stm :: Parser Stm
stm = Parsec.try stmA Parsec.<|> Parsec.try stmIf Parsec.<|> Parsec.try stmWhile

-- Statement parser for arithmetic expressions
stmA :: Parser Stm
stmA = do
    var <- identifier
    reservedOp ":="
    expr <- aexp
    semi
    return (Assign var expr)

thenstm :: Parser [Stm]
thenstm = parens (Parsec.many stm) Parsec.<|> liftM return stm

elsestm :: Parser [Stm]
elsestm = (parens (Parsec.many stm) <* semi) Parsec.<|> liftM return stm

-- Statement parser for if statements
stmIf :: Parser Stm
stmIf = do
    reserved "if"
    cond <- bexp
    reserved "then"
    stm1 <- thenstm
    reserved "else"
    stm2 <- elsestm
    return $ If cond (stm1) (stm2)

-- Statement parser for while loops
stmWhile :: Parser Stm
stmWhile = do
    reserved "while"
    cond <- bexp
    reserved "do"
    stm1 <- elsestm
    return $ While cond stm1

-- Parse a string into a list of statements
parse :: String -> [Stm]
parse str =
    case Parsec.parse (Parsec.spaces *> stms <* Parsec.eof) "" str of
        Left e  -> error $ show e
        Right r -> r

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

main :: IO()
main = do
  -- Examples:
  print $ testParser "x := 5; x := x - 1;" 
  print $ testParser "x := 0 - 2;" 
  print $ testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" 
  print $ testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);"
  print $ testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;"
  print $ testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;"
  print $ testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;"
  print $ testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;"
  print $ testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;"
  print $ testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;"
  print $ testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);"
  print $ testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);"

-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "x := 0 - 2;" == ("","x=-2")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")