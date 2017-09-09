import Data.List
import Data.Maybe

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

notFree :: Name -> Term -> Bool
notFree x t = not $ elem x (freeVars t)

-- Simple sub
-- sub :: Term -> Name -> Term -> Term
-- sub (Var x) y t2
--   | x == y    = t2
--   | otherwise = (Var x)
-- sub (App t1 t2) y t3 = (App (sub t1 y t3) (sub t2 y t3))
-- sub (Abs y t1) x t2
--   | x == y = (Abs y t1)
--   | not(elem x (vars t1)) && not(elem y (freeVars t2)) = (Abs y t1)
--   | otherwise = error "Undefined"

-- Задача 1.1
renameBoundVar :: Term -> Name -> Name -> Term
renameBoundVar t@(Var z)   x y = t
renameBoundVar (App t1 t2) x y = App (renameBoundVar t1 x y) (renameBoundVar t2 x y)
renameBoundVar (Abs z t) x y
  | z == x = (Abs y (renameBoundVar' t x y))
  | otherwise = (Abs z (renameBoundVar t x y))
  where
    renameBoundVar' t@(Var z) x y
      | x == z    = (Var y)
      | otherwise = t
    renameBoundVar' (App t1 t2) x y = App (renameBoundVar' t1 x y) (renameBoundVar' t2 x y)
    renameBoundVar' (Abs z t) x y
      | z == x    = (Abs y (renameBoundVar' t x y))
      | otherwise = (Abs z (renameBoundVar' t x y))

-- Задача 1.4
-- Advanced sub
sub :: Term -> Name -> Term -> Term
sub x'@(Var y) x n
  | y == x    = n
  | otherwise = x'
sub (App m1 m2) x n = App (sub m1 x n) (sub m2 x n)
sub m@(Abs y p) x n
  | y == x      = m
  | notFree y n = Abs y (sub p x n)
  | otherwise   = Abs fresh (sub renamed x n)
  where
    fresh = head $ (generator \\ (vars n)) \\ (vars p)
    renamed = renameBoundVar p y fresh

-- Should not result to identity
testSub = sub constant "y" (Var "x")

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
  where letters = ['x', 'y', 'z', 'u', 'v', 'w', 't']

-- generate :: [Name] -> Name
-- generate used = head (generator \\ used)
--
-- Задача 1.7 - функциите unname и name действайки върху множеството `generator`
unname :: Term -> UTerm
unname t = fromJust $ unname' [] (freeVars t) t
  where
    unname' bound free (Var x)         = UVar <$> elemIndex x (bound ++ free)
    unname' bound free (App exp1 exp2) = UApp <$> unname' bound free exp1 <*> unname' bound free exp2
    unname' bound free (Abs x exp)     = UAbs <$> unname' (x:bound) free exp

-- from DeBruijn index to Name
(!?) :: [a] -> Int -> Maybe a
xs !? n = listToMaybe (drop n xs)

name :: UTerm -> Term
name t = fromJust $ name' generator [] t
    where
        name'    names  bound (UVar x)         = Var   <$> (bound ++ names) !? x
        name'    names  bound (UApp exp1 exp2) = App   <$> name' names bound exp1 <*> name' names bound exp2
        name' (x:names) bound (UAbs exp)       = Abs x <$> name' names (x:bound) exp


(===) :: Term -> Term -> Bool
t1 === t2 = (unname t1) == (unname t2)
-- Проверки
-- • термът λxy се представя като λ1
-- • термът y(λxxyz)z се представя като 0(λ013)2,
-- • термът (λxxy(λuzu(λvvy))z) се представя като (λ01(λ40(λ03))3).
-- • термът λ(λ0((λ1)21))1 отговаря на λx(λtt((λut)yx))y.

x = Var "x"
y = Var "y"
z = Var "z"

term = (App (App y (Abs "x" (App x (App y z)))) z)

-- Изместване(ще помогне за субституцията)
shift :: Int -> Int -> UTerm -> UTerm
shift d c (UVar k)
  | k < c     = (UVar k)
  | otherwise = (UVar (k + d))
shift d c (UApp t1 t2) = (UApp (shift d c t1) (shift d c t2))
shift d c (UAbs t) = (UAbs (shift d (c + 1) t))

shiftZero :: Int -> UTerm -> UTerm
shiftZero d t = shift d 0 t

shiftWithOne :: UTerm -> UTerm
shiftWithOne = shiftZero 1

-- Дефиниция 1.5 (Субституция на безименни термове). Нека M, N ∈ Λn и
-- k ∈ N. С индукция по M дефинираме субституцията M[k → N] ∈ Λn.
-- (1) k[k → N] := N
-- (2) i[k → N] := i за i 6= k
-- (3) (M1M2)[k → N] := (M1[k → N])(M2[k → N])
-- (4) (λM)[k → N] := λ(M[k + 1 →↑1
-- (N)])
-- Задача 1.10 - M[k -> N]
subUnnamed :: UTerm -> Int -> UTerm -> UTerm
subUnnamed m@(UVar i) k n
  | i == k    = n
  | otherwise = m
subUnnamed (UApp t1 t2) k n = UApp (subUnnamed t1 k n) (subUnnamed t2 k n)
subUnnamed (UAbs m) k n = UAbs (subUnnamed m (k + 1) (shiftWithOne n))
