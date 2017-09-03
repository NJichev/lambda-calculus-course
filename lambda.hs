-- Lambda Terms
-- data Var = X | Y
-- data LambdaTerm = Var
--                 | Application LambdaTerm LambdaTerm
--                 | Abstraction Var LambdaTerm
--
-- term1 :: LambdaTerm
-- term1 = X
--
-- term2 :: LambdaTerm
-- term2 = Var Y
--
-- term3 :: LambdaTerm
-- term3 = Application term1 term2
--
data Name = Var String

data Term = Name
          | App Term Term
          | Abs Name Term
          deriving(Eq, Show)
