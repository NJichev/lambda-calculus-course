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
data LambdaTerm = Var
                | Application LambdaTerm LambdaTerm
                | Abstraction Var LambdaTerm


data Formula = Var String 
             | Not Formula 
             | Impl Formula Formula deriving (Eq)

type Interp = String -> Maybe Bool

eval :: Formula -> Interp -> Maybe Bool
eval (Var fi) i = i fi
eval (Not fi) i = (eval fi i) >>= notmonadic
eval _ _ = Nothing

notmonadic :: Bool -> Maybe Bool
notmonadic True = Just False
notmonadic False= Just True

ip :: Interp
ip "p1" = Just True
ip "p2" = Just True
ip "p3" = Just False
ip _ = Nothing
