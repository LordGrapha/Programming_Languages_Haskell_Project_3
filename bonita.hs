prioridad (Constante const)         = 6
prioridad (Variable var)            = 6
prioridad (Negacion prop)           = 5
prioridad (Conjuncion prop1 prop2) = 4
prioridad (Disyuncion prop1 prop2) = 3
prioridad (Implicacion prop1 prop2) = 2
prioridad (Equivalencia prop1 prop2) = 1


isImpli (Implicacion p q) = True
isImpli prop = False

isEqui (Equivalencia p q) = True
isEqui prop = False

bonita (Constante const)         = if const then "True" else "False"
bonita (Variable var)            = var
bonita (Negacion prop)           = 
  let 
    priori = prioridad prop
  in
    "~" ++ (if priori < 5 then "(" ++ bonita prop ++ ")" else bonita prop)
bonita prop = 
  if isConj prop
    then 
      (if prioridad (getFirst prop) < prioridad prop then "(" ++ bonita (getFirst prop) ++ ")" else bonita (getFirst prop))
      ++ " && " ++ 
      (if prioridad (getSecond prop) > prioridad prop then bonita (getSecond prop) else "(" ++ bonita (getSecond prop) ++ ")")
  else if isDisy prop
    then
      (if prioridad (getFirst prop) < prioridad prop then "(" ++ bonita (getFirst prop) ++ ")" else bonita (getFirst prop))    
      ++ " || " ++
      (if prioridad (getSecond prop) > prioridad prop then bonita (getSecond prop) else "(" ++ bonita (getSecond prop) ++ ")")
  else if isImpli prop
    then
      (if prioridad (getFirst prop) > prioridad prop then bonita (getFirst prop) else "(" ++ bonita (getFirst prop) ++ ")")
      ++ " => " ++
      (if prioridad (getSecond prop) < prioridad prop then "(" ++ bonita (getSecond prop) ++ ")" else bonita (getSecond prop)) 
  else if isEqui prop
    then
      bonita prop1 
      ++ " <=> " ++ 
      (if prioridad (getSecond prop) > prioridad prop then bonita (getSecond prop) else "(" ++ bonita (getSecond prop) ++ ")")
  else
    ""   
