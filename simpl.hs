isConst (Constante const) = True
isConst prop = False

isConj (Conjuncion p q) = True
isConj prop = False

isDisy (Disyuncion p q) = True
isDisy prop = False

isNeg (Negacion p) = True
isNeg prop = False

getFirst (Negacion p) = p
getFirst (Conjuncion p q) = p
getFirst (Disyuncion p q) = p
getFirst (Implicacion p q) = p
getFirst (Equivalencia p q) = p

getSecond (Conjuncion p q) = q
getSecond (Disyuncion p q) = q
getSecond (Implicacion p q) = q
getSecond (Equivalencia p q) = q

getConst (Constante const) = const

-- Algoritmo
-- Se busca una regla por aplicar, si se aplica una regla, entonces se obtiene una nueva prop y se llama de nuevo a simpl
-- Si no se puede aplicar una regla, se toman las dos props y se hace doble llamada recursiva a simpl unida por el simbolo de la prop
-- Cuando ya la prop no se puede partir, se devuelve la prop que haya en ese momento

simpl (Constante const) = Constante const
simpl (Variable var)    = Variable var
simpl (Negacion p)      = 
  let 
    disyuncion = isDisy p
    conjuncion = isConj p
  in
    if disyuncion
      then Conjuncion (Negacion (getFirst p)) (Negacion (getSecond p)) -- 1. DeMorgan ¬(P ∨ Q) ≡ ¬P ∧ ¬Q
    else if conjuncion
      then Disyuncion (Negacion (getFirst p)) (Negacion (getSecond p)) -- 2. DeMorgan ¬(P ∧ Q) ≡ ¬P ∨ ¬Q
    else if isNeg p 
      then simpl p -- 3. Doble negacion ¬¬P ≡ P
    else
      Negacion (simpl p)
simpl (Implicacion p q) = simpl (Disyuncion (Negacion p) q) -- 4. Implicacion y disyuncion
simpl (Conjuncion p q)  = 
  let
    iguales = p == q
  in
    if iguales 
      then simpl p -- 5. Idempotencia conjuncion
    else 
      if isConst q 
        then 
          if getConst q == True 
            then simpl p -- 6. Neutro P ∧ V0 ≡ P
          else q -- 7. Dominacion P ∧ F0 ≡ F0
      else if isConst p 
        then 
          if getConst p == True 
            then simpl q -- 8. Neutro V0 ∧ Q ≡ Q
          else p -- 9. Dominacion F0 ∧ Q ≡ F0
      else simpl p /\ simpl q
simpl (Disyuncion p q) = 
  let 
    iguales = p == q
  in
    if iguales 
      then simpl p -- 10. Idempotencia disyuncion
    else 
      if isConst q 
        then 
          if getConst q == False 
            then simpl p -- 11. Neutro P ∨ F0 ≡ P
          else q -- 12. Dominacion P ∨ V0 ≡ V0
      else if isConst p 
        then 
          if getConst p == False 
            then simpl q -- 13. Neutro F0 ∨ Q ≡ Q
          else p -- 14. Dominacion V0 ∨ Q ≡ V0
      else simpl p \/ simpl q