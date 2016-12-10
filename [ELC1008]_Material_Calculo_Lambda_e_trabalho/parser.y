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

TLam : lam var '.' TLam             { Abs $2 ( $4 ) }
       | lam var '.' '('TLam')'         { Abs $2 ( $5 ) }
   	   | '(' lam var '.' TLam')'        { ( Abs $3 ( $5 ) ) }		
	   | '(' lam var '.' '('TLam')' ')' { ( Abs $3 ( $6 ) ) }
	   | TLam TLam                  { ( App $1 $2 ) }
	   | '(' TLam TLam ')'          { ( App $2 $3 ) }
	   | '(' TLam ')' '(' TLam ')'  { ( App $2 $5 ) }
	   | var                                { Var $1 }

{

parseError :: [Token] -> a
parseError b = error "Parse Error"

data TLam = Var Char
           | Abs Char TLam
           | App TLam TLam deriving Show 

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
