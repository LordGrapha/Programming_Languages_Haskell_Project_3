-- ambiente :  [("q",True),("p",False),("r",True)]
busca ident [] = error "AMBIENTE VACÍO EN LA FUNCION *BUSCA*"  
busca ident ((llave,valor):ambiente)
  | ident == llave  = valor
  | otherwise       = busca ident ambiente


-- ambiente :  [("q",True),("p",False),("r",True)]
evalProp ambiente (Constante valor)           = valor
evalProp ambiente (Variable var)              = busca var ambiente
evalProp ambiente (Negacion prop)             = not (evalProp ambiente prop)
evalProp ambiente (Conjuncion prop1 prop2)    = evalProp ambiente prop1 && (evalProp ambiente prop2)
evalProp ambiente (Disyuncion prop1 prop2)    = evalProp ambiente prop1 || (evalProp ambiente prop2)
evalProp ambiente (Implicacion prop1 prop2)   = evalProp ambiente prop1 <= (evalProp ambiente prop2)
evalProp ambiente (Equivalencia prop1 prop2)  = evalProp ambiente prop1 == (evalProp ambiente prop2)

