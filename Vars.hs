module Vars where

-- ------------------------------------------------------------------
find _ [] = False
find n (x:xs)
  | x == n = True
  | otherwise = find n xs

-- ------------------------------------------------------------------
removeDups [] = []
removeDups [x] = [x]
removeDups (x:xs)
  | find x xs = removeDups xs
  | otherwise = x:removeDups xs

-- ------------------------------------------------------------------
getVars (Constante const)         = []
getVars (Variable var)            = [var]
getVars (Negacion prop)           = getVars prop
getVars (Conjuncion prop1 prop2) = getVars prop1 ++ getVars prop2
getVars (Disyuncion prop1 prop2) = getVars prop1 ++ getVars prop2
getVars (Implicacion prop1 prop2) = getVars prop1 ++ getVars prop2
getVars (Equivalencia prop1 prop2) = getVars prop1 ++ getVars prop2

-- ------------------------------------------------------------------
vars prop =
  let 
    variables = getVars prop
  in
    removeDups variables




    


