module Exercise6 where
import Exercise2
import LTS
import Data.List
import Test.QuickCheck

{-- Implementations, indication of time spent. --}


-- printStates :: [State] -> IO ()
-- printStates [] = putStrLn ""
-- printStates (x:xs) = do
--     putStr (show x ++ "        ")  
--     printStates xs  


-- visualizeLTS :: IOLTS -> IO ()
-- visualizeLTS (q,_,_,t,_) = do
--     printStates q
--     let indices = map (\pre -> findIndex (\x -> x == pre) q) (map (\(pre, _, _) -> pre) t)
--     mapM_ (\index -> putStrLn (replicate (maybe "Not found" show index) "        ")) indices

-- main :: IO ()
-- main = do
--     putStrLn "IOLTS :"
--     lts <- generate ltsGen
--     print lts
--     putStrLn "Visualization :"
--     visualizeLTS lts