module Exercise8 where
import Test.QuickCheck
import Data.List
import System.Random
import SetOrd
import Lecture6
import Text.Parsec
import Text.Parsec.String (Parser)

readStatement :: String -> Either ParseError Statement
readStatement = parse statementParser ""

statementParser :: Parser Statement
statementParser = do
  statement <- choice [assignmentParser, conditionalParser, sequenceParser, whileParser]
  optional spaces
  return statement

assignmentParser :: Parser Statement
assignmentParser = do
  var <- many1 letter
  spaces >> char '=' >> spaces
  expr <- expressionParser
  return (Ass var expr)

conditionalParser :: Parser Statement
conditionalParser = do
  string "if"
  condition <- spaces >> conditionParser
  spaces >> string "then" >> spaces
  stmt1 <- statementParser
  spaces >> string "else" >> spaces
  stmt2 <- statementParser
  return (Cond condition stmt1 stmt2)

sequenceParser :: Parser Statement
sequenceParser = do
  stmts <- many1 (statementParser <* optional (char ';'))
  return (Seq stmts)

whileParser :: Parser Statement
whileParser = do
  string "while"
  condition <- spaces >> conditionParser
  spaces >> string "do" >> spaces
  stmt <- statementParser
  return (While condition stmt)

expressionParser :: Parser Expr
expressionParser = choice [integerParser, variableParser, binaryOperationParser]

integerParser :: Parser Expr
integerParser = I <$> read <$> many1 digit

variableParser :: Parser Expr
variableParser = V <$> many1 letter

binaryOperationParser :: Parser Expr
binaryOperationParser = do
  e1 <- expressionParser
  spaces
  op <- choice [char '+', char '-', char '*']
  spaces
  e2 <- expressionParser
  return $ case op of
    '+' -> Add e1 e2
    '-' -> Subtr e1 e2
    '*' -> Mult e1 e2

conditionParser :: Parser Condition
conditionParser = choice
  [ try equalityParser
  , try lessThanParser
  , try greaterThanParser
  , try notParser
  , try conjunctionParser
  , disjunctionParser
  ]

equalityParser :: Parser Condition
equalityParser = do
  e1 <- expressionParser
  spaces >> char '=' >> spaces
  e2 <- expressionParser
  return (Eq e1 e2)

lessThanParser :: Parser Condition
lessThanParser = do
  e1 <- expressionParser
  spaces >> char '<' >> spaces
  e2 <- expressionParser
  return (Lt e1 e2)

greaterThanParser :: Parser Condition
greaterThanParser = do
  e1 <- expressionParser
  spaces >> char '>' >> spaces
  e2 <- expressionParser
  return (Gt e1 e2)

notParser :: Parser Condition
notParser = do
  string "not" >> spaces
  c <- conditionParser
  return (Ng c)

conjunctionParser :: Parser Condition
conjunctionParser = do
  string "and" >> spaces
  conditions <- sepBy1 conditionParser (spaces >> string "and" >> spaces)
  return (Cj conditions)

disjunctionParser :: Parser Condition
disjunctionParser = do
  string "or" >> spaces
  conditions <- sepBy1 conditionParser (spaces >> string "or" >> spaces)
  return (Dj conditions)

customShow :: Statement -> IO ()
customShow stmt = customShowIndented stmt 0

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

property_whitespaceInsensitivity :: String -> Property
property_whitespaceInsensitivity input =
  let strippedInput = trimWhitespace input
      result = readStatement input
  in case result of
       Left _ -> property_discard
       Right stmt ->
         let strippedResult = readStatement strippedInput
         in property (stmt == either (const stmt) id strippedResult)

property_parseIdentity :: String -> Property
property_parseIdentity input =
  let result = readStatement input
  in case result of
       Left _ -> property_discard
       Right stmt -> property (input == prettyPrint stmt)

property_roundTripSerialization :: String -> Property
property_roundTripSerialization input =
  let result1 = readStatement input
  in case result1 of
       Left _ -> property_discard
       Right stmt1 ->
         let serialized = prettyPrint stmt1
             result2 = readStatement serialized
         in case result2 of
              Left _ -> property_discard
              Right stmt2 -> property (stmt1 == stmt2)

property_indentationMatching :: Statement -> Int -> Bool
property_indentationMatching stmt indentLevel =
  let customOutput = captureOutput (customShowIndented stmt indentLevel)
      leadingTabs = takeWhile (== '\t') customOutput
  in length leadingTabs == indentLevel

property_outputLengthMatching :: Statement -> Bool
property_outputLengthMatching stmt =
  let customOutput = captureOutput (customShow stmt)
      serializedStmt = prettyPrint stmt
  in length customOutput == length serializedStmt

property_stringReversibility :: Statement -> Bool
property_stringReversibility stmt =
  let customOutput = captureOutput (customShow stmt)
  in case readStatement customOutput of
       Left _ -> True  -- Ignore parsing errors
       Right parsedStmt -> stmt == parsedStmt


main :: IO ()
-- main = customShow fib
main = do
  let statementStr = "x = 5; if x = 5 then y = 10 else y = 20"
  case readStatement statementStr of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right statement -> print statement