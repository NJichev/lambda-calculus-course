data EFormula = EVar String
              | ENot EFormula
              | EImpl EFormula EFormula
              | EAnd EFormula EFormula
              | EOr EFormula EFormula
              | EEvery String EFormula
              | EExist String EFormula
              deriving (Eq)

data Formula = Var String
             | Not Formula
             | Impl Formula Formula
             | Every String Formula
             deriving (Eq)

intern :: EFormula -> Formula
intern (EVar str) = (Var str)
intern (ENot fi) = (Not (intern fi))
intern (EImpl fi psi) = (Impl (intern fi) (intern psi))
intern (EEvery str fi) = (Every str (intern fi))
intern (EOr fi psi) = (Impl (Not (intern fi)) (intern psi))
intern (EAnd fi psi) = (Not (Impl (intern fi) (Not (intern psi))))
intern (EExist str fi) = (Not (Every str (Not (intern fi))))

instance Show Formula where
  show = forToString

foldfor :: (String -> b) -> (b -> b) -> (b -> b -> b) ->
           (String -> b -> b) -> Formula -> b
foldfor f _ _ _ (Var x) = f x
foldfor f g h u (Not fi) = g $ foldfor f g h u fi
foldfor f g h u (Impl fi psi) = h (foldfor f g h u fi) (foldfor f g h u psi)
foldfor f g h u (Every x fi) = u x $ foldfor f g h u fi

forToString :: Formula -> String
forToString = foldfor (\x -> x)
                      (\fi -> "(" ++ "¬" ++ fi ++ ")")
                      (\fi psi -> "(" ++ fi ++ "→" ++ psi ++ ")")
                      (\x fi -> "(" ++ "∀" ++ x ++ fi ++ ")")

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
axioms = [isAxiom1, isAxiom2]

isAxiom1 :: Formula -> Bool
isAxiom1 (Impl (Impl fi1 (Impl psi1 xi1)) (Impl (Impl fi2 psi2) (Impl fi3 xi2))) = fi1 == fi2 && fi2 == fi3 &&
                          psi1 == psi2 &&
                          xi1 == xi2
isAxiom1 _ = False

isAxiom2 :: Formula -> Bool
isAxiom2 (Impl fi (Impl psi xi)) = fi == xi
isAxiom2 _ = False


--(~=) :: Formula -> Formula -> Bool
--(~=) (Var x) (Var y) = True
--(~=) (Not fi) (Not psi) = fi ~= psi
--(~=) (Impl fi1 psi1) (Impl fi2 psi2) = fi1 ~= fi2 &&
--                                       psi1 ~= psi2
--(~=) (Not fi) (Not psi) = fi ~= psi
--(~=) (Every x fi) (Every y psi) = x == y &&
--                                  fi ~= psi
--(~=) _ _ = False

type Formulae = [Formula]
type Hypotheses = Formulae

isDeduction :: Hypotheses -> Formulae -> Bool
isDeduction hyp fs = isDeduction' hyp [] fs

-- isDeduction :: [EFormula] -> [EFormula] -> Bool
-- isDeduction hyp fs = isDeduction' hyp' [] fs'
--   where
--     hyp' = map intern hyp
--     fs'  = map intern fs
--
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
