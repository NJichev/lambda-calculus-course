-- Use Integer for unlimited power(instead of Int)
ackermann :: Integer -> Integer -> Integer
ackermann 0 y = y + 1
ackermann x 0 = ackermann (x - 1) 1
ackermann x y = ackermann (x - 1) (ackermann x (y - 1))

-- from wiki:
-- ackermann 4 2 -- produces a number with 19k digits.
-- I just killed the repl.
