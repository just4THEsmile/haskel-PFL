-- Part 2
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Expr (buildExpressionParser)
import Text.Parsec.Expr( Operator(..), Assoc(..) )
import Control.Monad (liftM)
import qualified Text.Parsec.Token as Token

import Stack
import State
import LowLevelMachine as LLM

languageDef =
  emptyDef { Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = ["if", "then", "else", "while", "do", "not", "and", "True", "False"]
           , Token.reservedOpNames = ["+", "-", "*", ":=", "==", "<=", "and", "not"]
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
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
            | Le Aexp Aexp
            deriving (Eq, Show)

-- Stm: Statements
data Stm = Assign String Aexp
            | Seq Stm Stm
            | If Bexp Stm Stm
            | While Bexp Stm
            deriving (Eq, Show)

-- Compile Arithmetic Expressions
compA :: Aexp -> Code
compA (Num n) = [Push n]
compA (Var x) = [Fetch x]
compA (Main.Add a1 a2) = compA a1 ++ compA a2 ++ [LLM.Add] 
compA (Main.Mult a1 a2) = compA a1 ++ compA a2 ++ [LLM.Mult] 
compA (Main.Sub a1 a2) = compA a1 ++ compA a2 ++ [LLM.Sub]  


-- Compile Boolean Expressions
compB :: Bexp -> Code
compB Main.TT = [LLM.Tru]
compB Main.FF = [LLM.Fals]
compB (Main.Neg b) = compB b ++ [LLM.Neg]
compB (Main.And b1 b2) = compB b1 ++ compB b2 ++ [LLM.And]
compB (Main.Equ a1 a2) = compA a1 ++ compA a2 ++ [LLM.Equ]
compB (Main.Le a1 a2) = compA a1 ++ compA a2 ++ [LLM.Le]

-- Compile Statements
compStm :: Stm -> Code
compStm (Assign x a) = compA a ++ [LLM.Store x]
compStm (Seq s1 s2) = compStm s1 ++ compStm s2
compStm (If b s1 s2) = compB b ++ [LLM.Branch (compStm s1) (compStm s2)]
compStm (While b s) = [LLM.Loop (compB b) (compStm s)]

-- Compile Programs
compile :: [Stm] -> Code
compile [] = []
compile (s:ss) = compStm s ++ compile ss


aexp :: Parser Aexp
aexp = buildExpressionParser aOperators aTerm

aOperators = [ [Infix (reservedOp "*" >> return Main.Mult) AssocLeft]
             , [Infix (reservedOp "+" >> return Main.Add) AssocLeft]
              , [Infix (reservedOp "-" >> return Main.Sub) AssocLeft]
             ]

aTerm = parens aexp
     <|> liftM Var identifier
     <|> liftM Num integer

bexp :: Parser Bexp
bexp = buildExpressionParser bOperators bTerm

bOperators = [ [Prefix (reservedOp "not" >> return Main.Neg)]
             , [Infix (reservedOp "and" >> return Main.And) AssocLeft]
             ]

bTerm = parens bexp
     <|> (reserved "True" >> return Main.TT)
     <|> (reserved "False" >> return Main.FF)
     <|> (do a1 <- aexp
             reservedOp "=="
             a2 <- aexp
             return $ Main.Equ a1 a2)
     <|> (do a1 <- aexp
             reservedOp "<="
             a2 <- aexp
             return $ Main.Le a1 a2)


stm :: Parser Stm
stm = do reserved "if"
         b <- bexp
         reserved "then"
         s1 <- stm
         reserved "else"
         s2 <- stm
         return $ Main.If b s1 s2
  <|> do reserved "while"
         b <- bexp
         reserved "do"
         s <- stm
         return $ Main.While b s
  <|> do var <- identifier
         reservedOp ":="
         a <- aexp
         return $ Main.Assign var a
  <|> do s1 <- stm
         semi
         s2 <- stm
         return $ Main.Seq s1 s2

parseString :: String -> [Stm]
parseString str =
  case parse (whiteSpace >> many1 stm) "" str of
    Left e  -> error "Parsing error"
    Right r -> r

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parseString programCode), createEmptyStack, createEmptyState)

main :: IO()
main = do
    let testCase= "x := 1;"
    print $ testParser testCase

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")