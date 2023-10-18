module Exercise8 where
import Test.QuickCheck
import Data.List
import System.Random
import SetOrd
import Lecture6
import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Char (isSpace)
import Data.List.Split
import System.IO

-- Time Spent: 90 minutes

stmtParser :: Parser Statement
stmtParser = do
  statement <- choice [assParser, condParser, sequenceParser, whileParser]
  optional spaces
  return statement

assParser :: Parser Statement
assParser = do
  var <- many1 letter
  spaces >> char '=' >> spaces
  expr <- exprParser
  return (Ass var expr)

condParser :: Parser Statement
condParser = do
  string "if"
  condition <- spaces >> conditionParser
  spaces >> string "then" >> spaces
  stmt1 <- stmtParser
  spaces >> string "else" >> spaces
  stmt2 <- stmtParser
  return (Cond condition stmt1 stmt2)

sequenceParser :: Parser Statement
sequenceParser = do
  stmts <- many1 (stmtParser <* optional (char ';'))
  return (Seq stmts)

whileParser :: Parser Statement
whileParser = do
  string "while"
  condition <- spaces >> conditionParser
  spaces >> string "do" >> spaces
  stmt <- stmtParser
  return (While condition stmt)

exprParser :: Parser Expr
exprParser = choice [numParser, varParser, operationsParser]

numParser :: Parser Expr
numParser = I <$> read <$> many1 digit

varParser :: Parser Expr
varParser = V <$> many1 letter

operationsParser :: Parser Expr
operationsParser = do
  e1 <- exprParser
  spaces
  op <- choice [char '+', char '-', char '*']
  spaces
  e2 <- exprParser
  return $ case op of
    '+' -> Add e1 e2
    '-' -> Subtr e1 e2
    '*' -> Mult e1 e2

conditionParser :: Parser Condition
conditionParser = choice
  [ try equalityParser
  , try lessThanParser
  , try greaterThanParser
  , try ngParser
  , try cjParser
  , djParser
  ]

equalityParser :: Parser Condition
equalityParser = do
  e1 <- exprParser
  spaces >> char '=' >> spaces
  e2 <- exprParser
  return (Eq e1 e2)

lessThanParser :: Parser Condition
lessThanParser = do
  e1 <- exprParser
  spaces >> char '<' >> spaces
  e2 <- exprParser
  return (Lt e1 e2)

greaterThanParser :: Parser Condition
greaterThanParser = do
  e1 <- exprParser
  spaces >> char '>' >> spaces
  e2 <- exprParser
  return (Gt e1 e2)

ngParser :: Parser Condition
ngParser = do
  string "not" >> spaces
  c <- conditionParser
  return (Ng c)

cjParser :: Parser Condition
cjParser = do
  string "and" >> spaces
  conditions <- sepBy1 conditionParser (spaces >> string "and" >> spaces)
  return (Cj conditions)

djParser :: Parser Condition
djParser = do
  string "or" >> spaces
  conditions <- sepBy1 conditionParser (spaces >> string "or" >> spaces)
  return (Dj conditions)

customRead :: String -> Either ParseError Statement
customRead = parse stmtParser ""

customShow :: Statement -> IO ()
customShow stmt = customShowIndented stmt 0

-- mostly adds some indentation to prettify the printing
customShowIndented :: Statement -> Int -> IO ()
customShowIndented (Ass var expr) indentLevel =
    putStrLn $ replicate indentLevel '\t' ++ "Ass " ++ show var ++ " " ++ show expr
customShowIndented (Cond cond stmt1 stmt2) indentLevel = do
    putStrLn $ replicate indentLevel '\t' ++ "Cond (" ++ show cond ++ ") {"
    customShowIndented stmt1 (indentLevel + 1)
    putStrLn $ replicate indentLevel '\t' ++ "} else {"
    customShowIndented stmt2 (indentLevel + 1)
    putStrLn $ replicate indentLevel '\t' ++ "}"
customShowIndented (Seq stmts) indentLevel = do
    putStrLn $ replicate indentLevel '\t' ++ "Seq ["
    mapM_ (\s -> customShowIndented s (indentLevel + 1)) stmts
    putStrLn $ replicate indentLevel '\t' ++ "]"
customShowIndented (While cond stmt) indentLevel = do
    putStrLn $ replicate indentLevel '\t' ++ "While (" ++ show cond ++ ") {"
    customShowIndented stmt (indentLevel + 1)
    putStrLn $ replicate indentLevel '\t' ++ "}"

main :: IO ()
-- main = customShow fib
main = do
  let statementStr = "x = 2; y = 3"
  let statements = splitStatements statementStr
  iterateStatements statements

-- tokenize statements string based on semicolon delimeter
splitStatements :: String -> [String]
splitStatements input = splitOn ";" input

-- parse each statement
-- for some reason, it is stuck after the first parsed statement
-- but we do not focus a lot on that, since the task is to parse only one statement.
iterateStatements :: [String] -> IO ()
iterateStatements [] = putStrLn "No more statements."
iterateStatements (token:rest) = do
  putStrLn $ "Token: " ++ token
  case customRead token of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right statement -> customShow statement
  putStrLn "Press Enter to continue"
  _ <- hFlush stdout  
  _ <- getLine
  iterateStatements rest