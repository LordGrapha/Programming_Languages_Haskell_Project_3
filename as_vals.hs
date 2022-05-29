{- as_vals seria:
as_vals [variable("p"), variable("r")] [true, false]
val it = [(variable "p",true),(variable "r",false)] :
(Proposicion * bool) list
  -}
as_vals :: [a] -> [b] -> [(a,b)]
as_vals (a:as) (b:bs) = (a,b) : as_vals as bs
as_vals _      _      = []

main :: IO ()
main = print a where a = as_vals ['a', 'b'] [True, False]