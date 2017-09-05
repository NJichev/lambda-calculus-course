import Data.List

type Name = String
type Names = [Name]

data EFormula = EVar Name
              | ENot EFormula
              | EImpl EFormula EFormula
              | EAnd EFormula EFormula
              | EOr EFormula EFormula
              | EEvery Name EFormula
              | EExist Name EFormula
              deriving (Eq)

data Formula = Var Name
             | Not Formula
             | Impl Formula Formula
             | Every Name Formula
             deriving (Eq)

-- Vars
freeVars :: Formula -> Names
freeVars (Var var)     = [var]
freeVars (Not fi)      = freeVars fi
freeVars (Impl fi psi) = (freeVars fi) ++ (freeVars psi)
freeVars (Every x fi)  = (freeVars fi) \\ [x]

boundVars :: Formula -> Names
boundVars (Var _)       = []
boundVars (Not fi)      = boundVars fi
boundVars (Impl fi psi) = (boundVars fi) ++ (boundVars psi)
boundVars (Every x fi)  = (boundVars fi) ++ [x]

vars :: Formula -> Names
vars fi = freeVars(fi) ++ boundVars(fi)


intern :: EFormula -> Formula
intern (EVar str) = (Var str)
intern (ENot fi) = (Not (intern fi))
intern (EImpl fi psi) = (Impl (intern fi) (intern psi))
intern (EEvery str fi) = (Every str (intern fi))
intern (EOr fi psi) = (Impl (Not (intern fi)) (intern psi))
intern (EAnd fi psi) = (Not (Impl (intern fi) (Not (intern psi))))
intern (EExist str fi) = (Not (Every str (Not (intern fi))))

instance Show Formula where
  show (Var var) = var
  show (Not fi) = "(" ++ "¬" ++ show fi ++ ")"
  show (Impl fi psi) = "(" ++ show fi ++ "→" ++ show psi ++ ")"
  show (Every var fi) = "(" ++ "∀" ++ var ++ show(fi) ++ ")"

instance Show EFormula where
  show (EVar var) = var
  show (ENot fi) = "(" ++ "¬" ++ show fi ++ ")"
  show (EImpl fi psi) = "(" ++ show fi ++ "→" ++ show psi ++ ")"
  show (EAnd fi psi) = "(" ++ show fi ++ "∧" ++ show psi ++ ")"
  show (EOr fi psi) = "(" ++ show fi ++ "∨" ++ show psi ++ ")"
  show (EExist var fi) = "(" ++ "∃" ++ var ++ show(fi) ++ ")"
  show (EEvery var fi) = "(" ++ "∀" ++ var ++ show(fi) ++ ")"

eq :: EFormula -> Formula -> Bool
eq efor for = (intern efor) == for

isAxiom :: Formula -> Bool
isAxiom f = or (map ($ f) axioms)

axioms :: [Formula -> Bool]
axioms = [isImplAxiom1, isImplAxiom2]

-- Axioms for ->
-- 1. (A → B → C) → (A → B) → A → C
isImplAxiom1 :: Formula -> Bool
isImplAxiom1 (Impl (Impl fi1 (Impl psi1 xi1)) (Impl (Impl fi2 psi2) (Impl fi3 xi2))) = fi1 == fi2 && fi2 == fi3 &&
                               psi1 == psi2 &&
                               xi1 == xi2
isImplAxiom1 _ = False

-- 2. A → B → A
isImplAxiom2 :: Formula -> Bool
isImplAxiom2 (Impl fi (Impl psi xi)) = fi == xi
isImplAxiom2 _ = False

-- Axioms for && converted to -> and !
-- 3 A ∧ B → A, A ∧ B → B
isAndAxiom1 :: Formula -> Bool
isAndAxiom1 (Impl (Not (Impl fi (Not psi))) xi) = fi == xi || psi == xi
isAndAxiom1 _ = False

-- 4 A → B → A ∧ B
isAndAxiom2 :: Formula -> Bool
isAndAxiom2 (Impl (Impl fi1 psi1) (Not (Impl fi2 (Not psi2)))) = fi1 == fi2 && psi1 == psi2
isAndAxiom2 _ = False

--
--5 A → A ∨ B, B → A ∨ B
isOrAxiom1 :: Formula -> Bool
isOrAxiom1 (Impl fi (Impl (Not psi) xi)) = fi == psi || fi == xi
isOrAxiom1 _ = False

--6 ((A → C) → (B → C)) → ((A ∨ B) → C)
isOrAxiom2 :: Formula -> Bool
isOrAxiom2 (Impl (Impl (Impl a1 c1) (Impl b1 c2)) (Impl (Impl (Not a2) b2) c3)) = a1 == a2 && b1 == b2 && c1 == c2 && c2 == c3
isOrAxiom2 _ = False


-- 7 ∀xA → A[x 7→ t]
isEveryAxiom1 :: Formula -> Bool
isEveryAxiom1 (Impl (Every str fi) psi) = any (\ new -> (simpleRename fi str new) == psi) . vars $ psi
isEveryAxiom1 _ = False

-- Rename everything to check for equality in 7.
simpleRename :: Formula -> Name -> Name -> Formula
simpleRename (Var name)    old new
  | old == name = Var new
  | otherwise   = Var name
simpleRename (Not fi)      old new = Not (simpleRename fi old new)
simpleRename (Impl fi psi) old new = Impl (simpleRename fi old new) (simpleRename psi old new)
simpleRename (Every x fi)  old new
  | old == x  = (Every new (simpleRename fi old new))
  | otherwise = (Every x (simpleRename fi old new))

-- 8 ∀x (B → A) → (B → ∀xA), ако x ∈/ FV(B)
isEveryAxiom2 :: Formula -> Bool
isEveryAxiom2 (Impl (Every x (Impl b1 a1)) (Impl b2 (Every y a2))) = x == y && b1 == b2 && a1 == a2 && (not $ elem x (freeVars b2))
isEveryAxiom2 _ = False

-- 9 A[x 7→ t] → ∃xA
isExistAxiom1 :: Formula -> Bool
isExistAxiom1 (Impl b (Not (Every x (Not a)))) = any (\ t -> (simpleRename a x t) == b) . vars $ b
isExistAxiom1 _ = False

-- 10 ∀x (A → B) → (∃xA → B), ако x ∈/ FV(B)
isExistAxiom2 :: Formula -> Bool
isExistAxiom2 (Impl (Every x (Impl a1 b1)) (Impl (Not (Every y (Not a2))) b2)) = x == y && a1 == a2 && b1 == b2 && (not $ elem x (vars b1))
isExistaxiom2 _ -> False

-- Test vars for the axioms
a = EVar "a"
b = EVar "b"

test1 = intern $ (EImpl (EAnd a b) a)
test2 = intern $ (EImpl (EAnd a b) b)

-- This tells us whether two formulas have the same structure
(~=) :: Formula -> Formula -> Bool
(~=) (Var x) (Var y) = True
(~=) (Not fi) (Not psi) = fi ~= psi
(~=) (Impl fi1 psi1) (Impl fi2 psi2) = fi1 ~= fi2 &&
                                       psi1 ~= psi2
(~=) (Every x fi) (Every y psi) = x == y &&
                                  fi ~= psi
(~=) _ _ = False

type Formulae = [Formula]
type Hypotheses = Formulae

isDeduction :: Hypotheses -> Formulae -> Bool
isDeduction hyp fs = isDeduction' hyp [] fs

isDeduction' :: Hypotheses -> Formulae -> Formulae -> Bool
isDeduction' hyp proved [] = True
isDeduction' hyp proved (f:fs)
  | isAxiom f              = proveNext
  | elem f hyp             = proveNext
  | isModusPonens proved f = proveNext
  | otherwise              = False
  where proveNext = isDeduction' hyp (f : proved) fs

-- isTautology :: [EFormula] -> Bool
-- isTautology = isDeduction []
--
isModusPonens :: Formulae -> Formula -> Bool
isModusPonens proved f = any (\ (Impl x _) -> elem x proved) impls
  where impls = [impl | impl@(Impl _ fi) <- proved,
                        fi == f]

x = Var "x"
target = (Impl x x)

proof = [
  (Impl x (Impl target x)),
  (Impl (Impl x (Impl target x)) (Impl (Impl x target) (Impl x x))),
  (Impl (Impl x target) (Impl x x)),
  (Impl x (Impl x x)),
  target
  ]
