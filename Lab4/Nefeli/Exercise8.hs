module Exercise8 where
import Test.QuickCheck
import Data.List
import System.Random
import SetOrd
import Lecture6

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

main :: IO ()
main = customShow fib