module Main where
--module Lam where
import ParserLamda

--data TLam	= Abs Char Exp		-- abstracao
--		| App TLam Exp		-- aplicacao
--		| Exp Exp
--		deriving (Show)

--data Exp	= Var Char		-- variaveis
--		| Brk TLam		-- parenteses
--                deriving (Show) 

removeChar :: Char -> [Char] -> [Char]
removeChar x [] = []
removeChar x a = if(x==head a) then (tail a) else (head a):(removeChar x (tail a))

removeBrk :: Exp -> TLam
removeBrk (Brk (t)) = t
removeBrk t = Exp(t)

freeVariables :: TLam -> [Char]
freeVariables (Exp v) = (freeVariablesExp v)
freeVariables (Abs x t) = removeChar x (freeVariablesExp t)
freeVariables (App t1 t2) = freeVariables(t1)++freeVariablesExp(t2)

freeVariablesExp :: Exp -> [Char]
freeVariablesExp (Var x) = [x]
freeVariablesExp (Brk t) = freeVariables(t)

isVal :: TLam -> Bool
isVal (Exp v) = isValExp(v)
isVal (Abs x t) = True
isVal _ = False

isValExp :: Exp -> Bool
isValExp (Var x) = True
isValExp (Brk t) = isVal(t)

eval :: TLam -> TLam
eval (Exp v) = removeBrk(evalExp(v))
eval (App (Exp (Brk v)) v2) = eval (App v v2)
eval (App (Abs x t12) v2) = removeBrk (subsExp (x, Exp(v2)) (t12))
eval (App t1 t2) =
	if not(isVal t1)
	then let t1' = eval(t1)
		in (App t1' t2)
	else let t2' = evalExp(t2)
		in (App t1 t2')

evalExp :: Exp -> Exp
evalExp (Var x) = (Var x)
evalExp (Brk t) = Brk(eval t)


subs :: (Char, TLam) -> TLam -> TLam
subs (x, s) (Exp t) = removeBrk(subsExp (x, s) t)
subs (x, t2) (Abs y t12) = 
	if (x==y) then Abs y t12
	else 
		if not(elem (y) (freeVariables(t2))) then Abs y (subsExp (x, t2) t12)
		else Abs y (subsExp (x, t2) t12)
subs (x, t2) (App t3 t4) = 
	App (subs(x, t2) t3) (subsExp(x, t2) t4)

subsExp :: (Char, TLam) -> Exp -> Exp
subsExp (x, t2) (Var y) = if (x == y) then (Brk t2) else (Var y)
subsExp (x, t2) (Brk t12) = Brk (subs (x, t2) t12)

--isEqual :: TLam -> TLam -> Bool
--isEqual (Abs x t) (Abs y w) = True
--isEqual (Exp e) (Exp f) = True
--isEqual (App t x) (App w y) = True
--isEqual _ _ = False

--interpret :: TLam -> TLam
--interpret e
-- (isEqual e (eval e)) = e
-- otherwise = interpret (eval e)

main = getContents >>= print . interpret . calc . lexer

