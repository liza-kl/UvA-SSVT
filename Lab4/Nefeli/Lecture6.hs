
module Lecture6

where 

import Data.List
import Data.Char
import Test.QuickCheck

update :: Eq a => (a -> b) -> (a,b) -> a -> b
update f (x,y) = \ z -> if x == z then y else f z 

updates :: Eq a => (a -> b) -> [(a,b)] -> a -> b
updates = foldl update

infixl 1 $$

($$) :: a -> (a -> b) -> b
($$) = flip ($)

type Var = String
type Env = Var -> Integer

data Expr = I Integer | V Var 
          | Add Expr Expr 
          | Subtr Expr Expr 
          | Mult Expr Expr 
          deriving (Eq,Show)

eval :: Expr -> Env -> Integer 
eval (I i) _ = i 
eval (V name) env = env name
eval (Add e1 e2) env = (eval e1 env) + (eval e2 env)
eval (Subtr e1 e2) env = (eval e1 env) - (eval e2 env)
eval (Mult e1 e2) env = (eval e1 env) * (eval e2 env)

assign :: Var -> Expr -> Env -> Env 
assign var expr env =  update env (var, eval expr env)

initEnv :: Env 
initEnv = \ _ -> undefined

data Condition = Prp Var 
               | Eq Expr Expr 
               | Lt Expr Expr 
               | Gt Expr Expr 
               | Ng Condition 
               | Cj [Condition] 
               | Dj [Condition]
               deriving (Eq,Show)

data Statement = Ass Var Expr
               | Cond Condition Statement Statement
               | Seq [Statement]
               | While Condition Statement
               deriving (Eq,Show)

evalc :: Condition -> Env -> Bool
evalc (Eq e1 e2) env = eval e1 env == eval e2 env
evalc (Lt e1 e2) env = eval e1 env <  eval e2 env
evalc (Gt e1 e2) env = eval e1 env >  eval e2 env 
evalc (Ng c) env = not (evalc c env)
evalc (Cj cs) env = and (map (\ c -> evalc c env) cs)
evalc (Dj cs) env = or  (map (\ c -> evalc c env) cs)

exec :: Statement -> Env -> Env 
exec (Ass v e) env = assign v e env
exec (Cond c s1 s2) env = 
 if evalc c env then exec s1 env else exec s2 env 
exec (Seq ss) env = foldl (flip exec) env ss 
exec w@(While c s) env = 
 if not (evalc c env) then env 
 else exec w (exec s env) 

fib :: Statement
fib = Seq [Ass "x" (I 0), Ass "y" (I 1), 
           While (Gt (V "n") (I 0))
             (Seq [Ass "z" (V "x"), 
                   Ass "x" (V "y"),
                   Ass "y" (Add (V "z") (V "y")), 
                   Ass "n" (Subtr (V "n") (I 1))])]

fib' :: Statement
fib' = Seq [Ass "x" (I 0), Ass "y" (I 1),
           While (Prp "n")
             (Seq [Ass "z" (V "x"),
                   Ass "x" (V "y"),
                   Ass "y" (Add (V "z") (V "y")),
                   Ass "n" (Subtr (V "n") (I 1))])]

run :: [(Var,Integer)] -> Statement -> [Var] -> [Integer]
run xs program vars = 
  exec program (updates initEnv xs) $$ 
    \ env -> map (\c -> eval c env) (map V vars)

runFib n = run [("n",n)] fib ["x"]

while :: (a -> Bool) -> (a -> a) -> a -> a
while = until . (not.)

whiler :: (a -> Bool) -> (a -> a) -> (a -> b) -> a -> b
whiler p f r = r . while p f 

fibonacci :: Integer -> Integer
fibonacci n = fibon (0,1,n) where 
  fibon = whiler 
           (\ (_,_,n) -> n > 0)
           (\ (x,y,n) -> (y,x+y,n-1))
           (\ (x,_,_) -> x)

fp :: Eq a => (a -> a) -> a -> a 
fp f = until (\ x -> x == f x) f

fbo n = (0,1,n) $$ 
         fp (\ (x,y,k) -> if k == 0 then (x,y,k) else (y,x+y,k-1))

bab a = \ x -> ((x + a/x)/2)

sr a = fp (bab a) a

iterateFix :: Eq a => (a -> a) -> a -> [a]
iterateFix f = apprx . iterate f where
  apprx (x:y:zs) = if x == y then [x] else x: apprx (y:zs)

fix :: (a -> a) -> a
fix f = f (fix f)

fbx n = (0,1,n) $$ 
         fix (\ f (x,y,k) -> if k == 0 then x else f (y,x+y,k-1))

fbb n = fbbb (0,1,n) where 
  fbbb (x,y,n) = if n == 0 then x else fbbb (y,x+y,n-1)

fbc n = fbbc 0 1 n where 
  fbbc x y n = if n == 0 then x else fbbc y (x+y) (n-1)

fp' :: Eq a => (a -> a) -> a -> a
fp' f = fix (\ g x -> if x == f x then x else g (f x))

until' :: (a -> Bool) -> (a -> a) -> a -> a
until' p f = fix 
              (\ g x -> if p x then x else g (f x))

while' :: (a -> Bool) -> (a -> a) -> a -> a
while' p f = fix 
              (\ g x -> if not (p x) then x else g (f x))

data Color = W | B deriving (Eq,Show)

drawPebble :: [Color] -> [Color]
drawPebble [] = []
drawPebble [x] = [x]
drawPebble (W:W:xs) = drawPebble (B:xs) 
drawPebble (B:B:xs) = drawPebble (B:xs) 
drawPebble (W:B:xs) = drawPebble (W:xs) 
drawPebble (B:W:xs) = drawPebble (W:xs) 

drawPebbleList xs = (drawPebble xs, xs)

parityDrawPebbleList xs = (parityW (drawPebble xs), drawPebble xs, xs)

instance Arbitrary Color where
  arbitrary = oneof [return W, return B]

numberW :: [Color] -> Int
numberW = length . (filter (== W)) 
    
parityW :: [Color] -> Int
parityW xs =  mod (numberW xs) 2
    
prop_invariant xs = 
  parityW xs == parityW (drawPebble xs)

prop_length xs = length xs == length (drawPebble xs)

sampleF f g =
 do cases <- sample' g
    sequence_ (map (print.f) cases)
