module Main where
import ParserLambda

import Data.List

--Remove primeira ocorrência do caractere c na string s
removeFirstCharFound :: Char -> [Char] -> [Char]
removeFirstCharFound c [] = []
removeFirstCharFound c s = if c == head(s) then tail(s) else head s : removeFirstCharFound c (tail s)

findAndRenameVar :: TLam -> TLam
findAndRenameVar (App (Abs x t) (Var y)) = 
    --testa se o argumento da abstração é igual a variável livre
    --testa se tem uma variável ligada igual a variável livre
	if (x /= y) && (elem y (linkedVars t))
		then (App (Abs x (renameLinkedVar y t (nameNewVar t))) (Var y))
	else (App (Abs x t) (Var y))
		

renameLinkedVar :: Char -> TLam -> Char -> TLam
-- Substitui se variável for igual a x
renameLinkedVar x (Var y) z = if (x == y)
	then (Var z)
	else (Var y)
-- Substitui argumento da abstração se necessário e recursiona no termo
renameLinkedVar x (Abs y t) z = if (x == y)
	then (Abs z (renameLinkedVar x t z))
	else (Abs y (renameLinkedVar x t z))
-- Recursiona para cada termo da aplicação
renameLinkedVar x (App t1 t2) z = (App (renameLinkedVar x t1 z) (renameLinkedVar x t2 z)) 


nameNewVar :: TLam -> Char
nameNewVar t = head("abcdefghijklmnopqrstuvwxyz"  \\ freeVars(t))

--retorna todas variáveis ligadas do termo
linkedVars :: TLam -> [Char]
linkedVars (Var x) = []
linkedVars (Abs x t) = [x] ++ linkedVars(t)
linkedVars (App t1 t2) = linkedVars (t1) ++ linkedVars (t2)

--retorna todas variáveis livres
freeVars :: TLam -> [Char]
freeVars (Var x)     = [x]
freeVars (Abs x t)   = removeFirstCharFound x (freeVars t)
freeVars (App t1 t2) = freeVars t1 ++ freeVars t2

--aplica betaredução
beta_redex :: TLam -> TLam
beta_redex (App (Abs x t1) t2) = subs x (beta_redex(t2)) t1
beta_redex (App t1 t2) = (App (beta_redex(t1)) (beta_redex(t2)))
beta_redex (Var x) = (Var x)
beta_redex (Abs x t) = (Abs x (beta_redex(t)))

subs :: Char -> TLam -> TLam -> TLam
-- 
subs x s (Var y) = if (x == y) then s else (Var y)
subs x s (Abs y t1) = if (x /= y) then (Abs y (subs x s t1)) else (Abs y t1)
subs x s (App t1 t2) = App (subs x s t1) (subs x s t2) 

--converte de TLam Para String
tlamToString :: TLam -> [Char]
tlamToString (Var x) = [x]
tlamToString (Abs x t) = "( lam " ++ [x] ++ " . " ++ tlamToString(t) ++ " )"
tlamToString (App t1 t2) = "( " ++ tlamToString(t1) ++ " " ++ tlamToString(t2) ++ " )"


main = do
	putStr("Insera sua expressão lambda") 
	contents <- getContents
	putStr (tlamToString(beta_redex(findAndRenameVar(parserlamb (lexer contents)))))
