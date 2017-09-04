import Data.List

type Name = String

-- Our Lambda Terms(Named ones)
data Term = Var Name
          | App Term Term
          | Abs Name Term

instance Show Term where
  show (Var a) = a
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


-- Unnamed lambda terms
data UTerm = UVar Int
           | UApp UTerm UTerm
           | UAbs UTerm
           deriving (Eq, Read)

instance Show UTerm where
  show (UVar i) = show i
  show (UApp t1 t2) = show t1 ++ show t2
  show (UAbs term) = "(" ++ "λ" ++ " " ++ show term ++ ")"

generator :: [Name]
generator = [ [i] | i <- letters] ++ [i: show j | j <- [1..], i <- letters]
  where letters = ['x', 'y', 'z', 't', 'u', 'v']

generate :: [Name] -> Name
generate used = head (generator \\ used)

-- Проверки
-- • термът λxy се представя като λ1
-- • термът y(λxxyz)z се представя като 0(λ013)2,
-- • термът (λxxy(λuzu(λvvy))z) се представя като (λ01(λ40(λ03))3).
-- • термът λ(λ0((λ1)21))1 отговаря на λx(λtt((λut)yx))y.
--
-- name :: UTerm -> Term
-- name = name'
--   where
--     name' :: 
--
-- unname :: Term -> UTerm

-- We want
-- unname(\x -> x) == \ 0
