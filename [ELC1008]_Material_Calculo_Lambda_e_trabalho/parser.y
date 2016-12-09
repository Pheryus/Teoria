{
module ParserLambda where
import Data.Char
}

%name parserlamb
%tokentype { Token }
%error { parseError }

%token
	lam { TokenLam } 
	var { TokenVar $$ }
	'.' { TokenPoint }
	'(' { TokenOB }
	')' { TokenCB }
	
%%

-- regras de producao da gramatica

CalcLamb : lam var '.' CalcLamb             { LamAbs $2 ( $4 ) }
       | lam var '.' '('CalcLamb')'         { LamAbs $2 ( $5 ) }
   	   | '(' lam var '.' CalcLamb')'        { ( LamAbs $3 ( $5 ) ) }		
	   | '(' lam var '.' '('CalcLamb')' ')' { ( LamAbs $3 ( $6 ) ) }
	   | CalcLamb CalcLamb                  { ( LamApp $1 $2 ) }
	   | '(' CalcLamb CalcLamb ')'          { ( LamApp $2 $3 ) }
	   | '(' CalcLamb ')' '(' CalcLamb ')'  { ( LamApp $2 $5 ) }
	   | var                                { Var $1 }

{

parseError :: [Token] -> a
parseError b = error "Parse Error"

data CalcLamb 
		= LamAbs Char CalcLamb
		| LamApp CalcLamb CalcLamb
		| Var Char
	deriving Show

data Token 
		= TokenVar Char
		| TokenPoint
		| TokenOB
		| TokenCB
		| TokenLam 
	deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
	| areEqual (c:(take 2 cs)) "lam" = TokenLam : lexer (tail(tail(tail(cs))))
	| isSpace c = lexer cs
	| isAlpha c = TokenVar c : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs
lexer ('.':cs) = TokenPoint : lexer cs

areEqual :: String -> String -> Bool
areEqual a b = a == b

main = getContents >>= print . parserlamb .lexer

}
