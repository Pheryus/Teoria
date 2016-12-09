module LAM where
import ParserLambda

data TLam = Var Char
           | Abs Char TLam
           | App TLam TLam deriving Show 


remove :: Char -> [Char] -> [Char]
remove  = undefined           


freeVars :: TLam -> [Char]
freeVars (Var x)     = [x]
freeVars (Abs x t)   = remove x (freeVars t)
freeVars (App t1 t2) = freeVars t1 ++ freeVars t2

--semantica
beta_redex :: TLam -> TLam
beta_redex (App (Abs x t12) t2) = subs x t2 t12

subs :: Char -> TLam -> TLam -> TLam
subs x s (Var y) = if (x == y) then s else (Var y)
subs x s (Abs y t1) = (Abs y (subs x s t1))
subs x s (App t1 t2) = App (subs x s t1) (subs x s t2) 

main = getContents >>= print . beta_redex . parserlamb . lexer
