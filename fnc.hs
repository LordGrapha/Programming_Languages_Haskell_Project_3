fnc_disyunciones prop =
  let
    variables = vars prop
    n = length variables
    lista_combinaciones_booleanas = gen_bools n
    -- generar evaluaciones de la proposición
    recorrer []                  = []  -- toque final a la generación; ya fueron generadas las filas precedentes *)
    recorrer (fila:mas_filas) = 
      let
          -- establecer una asociación entre variables y una combinación de valores booleanos (fila)
          asociacion = as_vals variables fila -- [("p", true), ("q", true)]*)
          -- esta asociación constituye un ambiente, o contexto, para evaluar la proposición prop *)
          resultado_fila = evalProp asociacion prop
      in  
          -- AGREGA A UNA LISTA TODAS LAS COMBINACIONES BOOLEANAS (filas) QUE DAN FALSE *)
          if not resultado_fila 
            then [asociacion] ++ recorrer mas_filas -- continuar el trabajo
          else recorrer mas_filas
  in
    recorrer lista_combinaciones_booleanas


-- [("p", true), ("q", true)]
disyuntar [] = error "LISTA VACIA EN DISYUNTAR"
disyuntar (elemento:mas_elementos) =
  let 
    (var, valor) = elemento
    var2 = Variable (var) -- [(variable "p", true), (variable "q", true)]
    fVar = case valor of False  -> var2 
                         True -> Negacion var2
  in
    if length mas_elementos > 0 then fVar \/ disyuntar mas_elementos else fVar



fnc prop =
  let
    disyunciones = fnc_disyunciones prop -- [  [("p", true), ("q", true)] , [("p", true), ("q", false)], ...]
    -- ITERA SOBRE LISTA disyunciones Y VA UNIENDO CADA ELEMENTO DENTRO DE UNA SUBLISTA CON OR Y CADA SUBLISTA CON AND 
    recorrer [] = error "LA PROPOSICIÓN NO POSEE FORMA NORMAL DISYUNTIVA"
    recorrer (fila:mas_filas) = 
      let
        disy = disyuntar fila -- CREO UNA DISYUNCION CON LOS ELEMENTOS*)
      in  
        if length mas_filas > 0 then disy /\ (recorrer mas_filas) else disy
  in 
    recorrer disyunciones
