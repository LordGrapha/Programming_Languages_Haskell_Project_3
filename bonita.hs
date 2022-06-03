import Sintax

prioridad prop =
    case prop of
        Constante False -> 6
        Constante True -> 6
        Variable nombre -> 6
        Negacion prop1 -> 5
        Conjuncion prop1 prop2 -> 4
        Disyuncion prop1 prop2 -> 3
        Implicacion prop1 prop2 -> 2
        Equivalencia prop1 prop2 -> 1

bonita prop =
    case prop of
        Constante False -> "false"
        Constante True -> "true"
        Variable nombre -> nombre
        Negacion prop1 -> negacion_aux prop1
        Conjuncion prop1 prop2 -> conjuncion_aux prop prop1 prop2
        Disyuncion prop1 prop2 -> disyuncion_aux prop prop1 prop2
        Implicacion prop1 prop2 -> implicacion_aux prop prop1 prop2
        Equivalencia prop1 prop2 -> equivalencia_aux prop prop1 prop2
    where
        negacion_aux prop1 = "~" ++
            if prioridad prop1 < 5 
                then "(" ++ bonita prop1 ++ ")" 
                else bonita prop1
        conjuncion_aux prop prop1 prop2 =
            if prioridad prop1 < prioridad prop
                then  "(" ++ bonita prop1 ++ ")"
                else bonita prop1
            ++ "&&" ++
            if prioridad prop2 > prioridad prop
                then bonita prop2
                else "(" ++ bonita prop2 ++ ")"
        disyuncion_aux prop prop1 prop2 =
            if prioridad prop1 < prioridad prop
                then  "(" ++ bonita prop1 ++ ")"
                else bonita prop1
            ++ "||" ++
            if prioridad prop2 > prioridad prop
                then bonita prop2
                else "(" ++ bonita prop2 ++ ")"
        implicacion_aux prop prop1 prop2 =
            if prioridad prop1 > prioridad prop
                then  bonita prop1
                else "(" ++ bonita prop1 ++ ")"
            ++ "->" ++
            if prioridad prop2 < prioridad prop
                then "(" ++ bonita prop2 ++ ")"
                else bonita prop2
        equivalencia_aux prop prop1 prop2 = 
            bonita prop1
            ++ "<=>" ++
            if prioridad prop2 > prioridad prop
                then bonita prop2
                else "(" ++ bonita prop2 ++ ")"
{-
main :: IO()
main = print a where a = bonita prop1
-}