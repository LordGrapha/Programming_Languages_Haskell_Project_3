module Taut where
import Vars
import As_vals
import EvalProps
import Gen_bools
import Sintax (Proposicion (..), p)
taut :: Proposicion -> [Char]
taut prop =
    let
    variables = vars prop
    n = length variables
    lista_combinaciones_booleanas = head(map gen_bools [n])
    recorrer [] = True
    recorrer (fila : mas_filas) =
        let
            asociacion = as_vals variables fila
            evaluacion_es_verdadera = evalProp asociacion prop
        in if evaluacion_es_verdadera then recorrer mas_filas else error "No es tautologia"
    in if recorrer lista_combinaciones_booleanas then "Si es Tautologia" else "No es tautologia"

{-
prop5 = Disyuncion p (Constante False)
main :: IO()
main = print a where a = taut prop5
-}