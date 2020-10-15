import Data.Char
import Data.Map (empty, insert, (!))
import System.IO
import Text.Read

data Type    =  Tvar Int | Tfun Type Type                        deriving Eq
data Expr    =  Evar String | Eabs String Expr | Eapp Expr Expr  deriving Eq
data Rule    =  Equal Type Type
data Unifier =  Replace Int Type | TypeError

-- Pretty printing of expressions

always = True    -- False omits parentheses whenever possible

instance Show Expr where
  showsPrec p (Evar x) = (x ++)
  showsPrec p (Eabs x e) =
    showParen (always || p > 0) ((("\\" ++ x ++ ". ") ++) . showsPrec 0 e)
  showsPrec p (Eapp e1 e2) =
    showParen (always || p > 1) (showsPrec 1 e1 . (" " ++) . showsPrec 2 e2)

-- Parsing of expressions

instance Read Expr where
  readPrec = (do Ident x <- lexP
                 return (Evar x)) <++
             (do Punc "(" <- lexP
                 Punc "\\" <- lexP
                 Ident x <- lexP
                 Symbol "." <- lexP
                 e <- readPrec
                 Punc ")" <- lexP
                 return (Eabs x e)) <++
             (do Punc "(" <- lexP
                 e1 <- readPrec
                 e2 <- readPrec
                 Punc ")" <- lexP
                 return (Eapp e1 e2))

-- Pretty printing of types

instance Show Type where
  showsPrec p (Tvar alpha) = ("@" ++) . showsPrec 0 alpha
  showsPrec p (Tfun sigma tau) =
    showParen (p > 0) (showsPrec 1 sigma . (" -> " ++) . showsPrec 0 tau)

-- Pretty printing of rules
instance Show Rule where
  show (Equal t1 t2) = (show t1) ++ " := " ++ (show t2)

-- Type inference

inferType :: Expr -> (Int, Type, [Rule])
inferType e =
  let
    typeInferHelper g n (Evar x) = (n, Tvar (g ! x), [])
    typeInferHelper g n (Eabs x expr) = (n', Tfun (Tvar n) t, c)
      where (n', t, c) = typeInferHelper (insert x n g) (n+1) expr
    typeInferHelper g n (Eapp expr1 expr2) = (n2 + 1, exprType, (Equal t1 (Tfun t2 exprType)) : (c1 ++ c2))
      where (n1, t1, c1) = typeInferHelper g n expr1
            (n2, t2, c2) = typeInferHelper g n1 expr2
            exprType     = Tvar n2
  in
    typeInferHelper empty 0 e

-- Main program

readOne  =  do  s <- getLine
                let e = read s :: Expr
                -- putStrLn ("Parsed: " ++ show e)
                return e

count n m  =  sequence $ take n $ repeat m

main     =  do  n <- readLn
                expressions <- count n readOne
                print(expressions)
                print(inferType (expressions !! 4))
