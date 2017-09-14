-- data Lam = Abs (Lam -> Lam)
--          | App Lam Lam
--          | FV String
-- Задача 1.27. (5 т.) Да се предложи дефиниция на списъци в безтипово-
-- то λ-смятане. Да се реализират комбинатори реализиращи стандартните
-- функции map, foldr и filter.
-- Екстра кредит: (3 т.) Да се направи програмна реализация
-- Искаме:
-- data List a = Nil | Cons a (List a)
-- Cons 1 (Cons 2 (Cons 3 Nil))
--
-- Където foldr просто казва с какво да заменим
-- Cons и Nil.
-- Имаме само функции, искаме функция, която да представя активното поведение на списъците.
-- Ако напишем Cons 1 (Cons 2 (Cons 3 Nil))
-- използвайки c за Cons и n за nil
-- искаме c 1 (c 2 (c 3 n))

nil = \c n -> n -- К*

cons x xs = \c n -> c x (xs c n)

-- Проверка, че е същото
-- cons 1 (cons 2 (cons 3 nil))) =
-- cons 1 (cons 2 (cons 3 (\c n -> n)) =
-- cons 1 (cons 2 (\c n -> c 3 ((\c' n' -> n') c n))) =
-- cons 1 (cons 2 (\c n -> c 3 n)) =
-- cons 1 (\c n -> c 2 ((\c' n' -> c' 3 n') c n) ) =
-- cons 1 (\c n -> c 2 (c 3 n)) =
-- \c n -> c 1 ((\c' n' -> c' 2 (c' 3 n')) c n) =
-- \c n -> c 1 (c 2 (c 3 n))


list = cons 1 $ cons 2 $ cons 3 nil

showlist l = l ((++) . show) ""

foldr' f n l = l f n

map' f xs = foldr' (\x xs -> cons (f x) xs) nil xs


filter' p xs = foldr' (\x xs -> if p x then cons x xs else xs ) nil xs
