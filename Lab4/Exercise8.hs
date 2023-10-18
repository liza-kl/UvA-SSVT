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

{-

This exercise aims to write a parser for a simple, imaginary, imperative programming language.

Generally, a parser is made up of the following parts:

	- First, we do a lexical analysis of the given string of program code, which yields a list of tokens as the final result.
	- Then, we construct an abstract syntax tree using this list of tokens.
	- Using this tree, we can derive the different types of statements and which tokens, together, form, for example, an assignment, a condition or a while loop. Thus creating a list of statements from the given string of program code.
	
The lexical analyser uses pre-defined keywords, operators and separators, and self-chosen identifiers in special locations and looks for these kinds of values. (For example, by using regular expressions.)
Everything else could then be considered as a comment, for example.
Then, these values or composed values, which satisfy a particular rule, coming from a pre-defined grammar, are put into so-called Tokens.
This is done for the entire string, which results in a list of tokens. (* these things could also be modified by mutation testing to check up on aspects like lexical analysis and AST generation.)

From this list of tokens, we can derive a tree. A parent node from a tree contains the general syntactic structure, while its children have further information.
For example, Considering a while loop and how that would be constructed, this could be a parent "while" node, with its condition and body as its child nodes.
The condition and body could be further broken down like this until we arrive at the primary statements, identifiers, operators, etc.

Finally, when we apply this general tree construction process, we derive this abstract syntax tree from the given list of tokens.

Using this tree, we can derive the order of the statements and also how each statement is constructed from smaller sub-statements.

-}

-- Main statement parser
stmtParser :: Parser Statement
stmtParser = do
  statement <- choice [assParser, condParser, sequenceParser, whileParser]
  optional spaces
  return statement

-- Sub-Statement parsers
assParser :: Parser Statement
assParser = do
  var <- many1 letter
  spaces >> char '=' >> spaces
  Ass var <$> exprParser

condParser :: Parser Statement
condParser = do
  string "if"
  condition <- spaces >> conditionParser
  spaces >> string "then" >> spaces
  stmt1 <- stmtParser
  spaces >> string "else" >> spaces
  Cond condition stmt1 <$> stmtParser

sequenceParser :: Parser Statement
sequenceParser = do
  stmts <- many1 (stmtParser <* optional (char ';'))
  return (Seq stmts)

whileParser :: Parser Statement
whileParser = do
  string "while"
  condition <- spaces >> conditionParser
  spaces >> string "do" >> spaces
  While condition <$> stmtParser

varParser :: Parser Expr
varParser = V <$> many1 letter

exprParser :: Parser Expr
exprParser = choice [numParser, varParser, operationsParser]

numParser :: Parser Expr
numParser = I . read <$> many1 digit

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

greaterThanParser :: Parser Condition
greaterThanParser = do
  e1 <- exprParser
  spaces >> char '>' >> spaces
  Gt e1 <$> exprParser

ngParser :: Parser Condition
ngParser = do
  string "not" >> spaces
  Ng <$> conditionParser

equalityParser :: Parser Condition
equalityParser = do
  e1 <- exprParser
  spaces >> char '=' >> spaces
  Eq e1 <$> exprParser

lessThanParser :: Parser Condition
lessThanParser = do
  e1 <- exprParser
  spaces >> char '<' >> spaces
  Lt e1 <$> exprParser

-- Declarations for custom read and show functions
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
splitStatements = splitOn ";"

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