import Data.Char
import Data.Map (empty, insert, lookup, (!))
import System.IO
import Text.Read

data Type    =  Tvar Int | Tfun Type Type                        deriving Eq
data Expr    =  Evar String | Eabs String Expr | Eapp Expr Expr  deriving Eq
data Rule    =  Equal Type Type
data Unifier =  Replace Type Type | TypeError

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

-- Pretty printing of unifiers
instance Show Unifier where
  show TypeError = "type error"
  show (Replace t1 t2) =
    "replace (" ++ (show t1) ++ ") with (" ++ (show t2) ++ ")"

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

-- Unification of Rules using the Algorithm W

-- Tools
(Tvar x) `includes` (Tvar y) =  x == y
(Tfun t1 t2) `includes` t = (t1 `includes` t) || (t2 `includes` t)

isSimpleVar (Tvar _) = True
isSimpleVar _ = False

-- Applying functions to Types inside a Rule
-- (that could be the Functor implementation of Rule, but it's not a parametric type)
applyToRule :: (Type -> Type) -> Rule -> Rule
applyToRule f (Equal t1 t2) = Equal (f t1) (f t2)

-- Replacing a type
replace :: Type -> Type -> Type -> Type
replace t1 t2 (Tvar x) =
  if (Tvar x) == t1 then t2 else (Tvar x)
replace t1 t2 (Tfun a b) =
  if (Tfun a b) == t1 then t2 else (Tfun (replace t1 t2 a) (replace t1 t2 b))

unify :: [Rule] -> [Unifier]
unify [] = []
unify ((Equal t1 t2) : rs)
  | t1 == t2  = unify rs

  | (isSimpleVar t1) && (not $ t2 `includes` t1) =
    (Replace t1 t2) : unify (map (applyToRule $ replace t1 t2) rs)

  | (isSimpleVar t2) && (not $ t1 `includes` t2) =
    (Replace t2 t1) : unify (map (applyToRule $ replace t2 t1) rs)

  | otherwise = [TypeError]

-- Apply the unified rules
applyRules :: Type -> [Unifier] -> Maybe Type
applyRules t [] = Just t
applyRules t (TypeError : xs) = Nothing
applyRules t ((Replace t1 t2) : xs) = applyRules (replace t1 t2 t) xs

-- Re-assign varriable numbers
third (_, _, x) = x
fromJust (Just x) = x

reorder :: Type -> Type
reorder t =
  let
    reorderHelper usedVars nextVar (Tvar x) = (newUsedVars, newNextVar, Tvar num)
      where lookupResult = Data.Map.lookup x usedVars
            num = if lookupResult == Nothing then nextVar else fromJust lookupResult
            newNextVar = if lookupResult == Nothing then nextVar+1 else nextVar
            newUsedVars = if lookupResult == Nothing then (insert x num usedVars) else usedVars

    reorderHelper usedVars nextVar (Tfun a b) = (newUsedVars, newNextVar, Tfun a' b')
      where (map', var', a') = reorderHelper usedVars nextVar a
            (newUsedVars, newNextVar, b') = reorderHelper map' var' b
  in
    third $ reorderHelper empty 0 t

-- Full pipeline
typeInString expr =
  let
    (_, t, rules) = inferType expr
    unifiedRules = unify rules
    finalType = applyRules t unifiedRules
    reorderedVars = reorder <$> finalType
    typeInString = if reorderedVars == Nothing
                    then "type error"
                    else show $ fromJust reorderedVars
  in
    typeInString

-- Main program

readOne  =  do  s <- getLine
                let e = read s :: Expr
                -- putStrLn ("Parsed: " ++ show e)
                return e

count n m  =  sequence $ take n $ repeat m

main     =  do  n <- readLn
                expressions <- count n readOne
                mapM putStrLn (typeInString <$> expressions)
                -- print(expressions)
                -- print(inferType (expressions !! 4))
                -- let (_, t, rules) = inferType (expressions !! 4)
                -- let unifiedRules = unify rules
                -- let finalType = applyRules t unifiedRules
                -- print(unifiedRules)
                -- print(finalType)
                -- print( reorder <$> finalType )
