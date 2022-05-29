{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
gen_bools 0 = [[]]
gen_bools n = map (True :) anterior ++ map (False :) anterior
              where anterior = gen_bools (n - 1)
{- 
[2]
0- [[]]
1- [[True], [False], ]
2- [[True, True], [True, False], [False, True], [False, False]]
-}
test = map gen_bools [1]

main :: IO ()
main = print test

{- as_vals seria:

as_vals [variable("p"), variable("r")] [true, false]

val it = [(variable "p",true),(variable "r",false)] :
(Proposicion * bool) list
  
  -}