
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
