-- Lambda Terms
-- data Var = X | Y
-- data LambdaTerm = Var
--                 | Application LambdaTerm LambdaTerm
--                 | Abstraction Var LambdaTerm
--
-- t1 :: LambdaTerm
-- t1 = X
--
-- t2 :: LambdaTerm
-- t2 = Var Y
--
-- term3 :: LambdaTerm
-- term3 = Application t1 t2
--
import Data.List

type Name = String

data Term = Var Name
          | App Term Term
          | Abs Name Term

instance Show Term where
  show (Var a) = "'" ++ a ++ "'"
  show (App t1 t2) = "(" ++ show t1 ++ show t2 ++ ")"
  show (Abs var term) = "λ" ++ var ++ "(" ++ show term ++ ")"


-- Examples
-- x :: Term
-- x = Var "x"
identity :: Term
identity = Abs "x" (Var "x")

constant :: Term
constant = Abs "x" (Var "y")

-- end examples

freeVars :: Term -> [Name]
freeVars (Var var) = [var]
freeVars (App t1 t2) = freeVars(t1) ++ freeVars(t2)
freeVars (Abs var term) = freeVars(term) \\ [var]

boundVars :: Term -> [Name]
boundVars (Var var) = []
boundVars (App t1 t2) = boundVars(t1) ++ boundVars(t2)
boundVars (Abs var term) = boundVars(term) ++ [var]

vars :: Term -> [Name]
vars term = freeVars(term) ++ boundVars(term)

sub :: Term -> Name -> Term -> Term
sub (Var x) y t2
  | x == y    = t2
  | otherwise = (Var x)
sub (App t1 t2) y t3 = (App (sub t1 y t3) (sub t2 y t3))
sub (Abs y t1) x t2
  | x == y = (Abs y t1)
  | not(elem x (vars t1)) && not(elem y (freeVars t2)) = (Abs y t1)
  | otherwise = error "Undefined"


rename :: Term -> Name -> Term

